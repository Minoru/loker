module IR where
import Control.Monad
import qualified AST

----------------------------------------
-- Intermediate representation

data Statement
    -- recursive nodes
    = Sequence [Statement]
    | Subshell Statement
    | ApplyRedirections [Redirection] Statement
    | Pipeline [Statement]
    | And Statement Statement
    | Or  Statement Statement

    -- leaves
    | Command
        Array -- argv, including the command name, argv[0]
    | Assignment AST.Name Expression
    deriving Show

-- About Globbable and Splittable
-- ------------------------------
-- Since our IR doesn't contain quotes, we lose some information.
-- In particular, presence of quotes determine whether the part of the word is
-- subjected to field splitting and filename generation.
--
-- To represent this information, we have Splittable and Globbable constructors
-- of Expression datatype. 'Splittable e' represents an expression (a part of
-- the word) which is subjected to field splitting, while by default expression
-- is not. Ditto for Globbable.

data Expression
    = Parameter AST.Parameter
    | ConcatE [Expression]
    | Const String
    | Splittable Expression
    | Globbable Expression
    -- todo: arithmetics
    deriving Show

-- Array
-- -----
-- 'Array' is actually an array of fields which form simple command.
-- Not all fields (and even treir number) are known during compile time because
-- of field splitting and filename generation. That's why we have constructors
-- Split and Glob, which represent an array which results from applying
-- apropriate procedure to an expression.
--
-- If there is an unquoted parameter substitution, it is subjected both to field
-- splitting and filename generation. So we need something like (in Haskell
-- pseudocode)
--
--   map Glob (Split expr)
--
-- SplitGlob constructor represents exactly this. It seems that there are no
-- cases when field splitting should occur and filename generation should not,
-- so Split constructor seems redundant. However, during analysis we may prove
-- that there are no pattern symbols in the variable, and we then will replace
-- SplitGlob with a simple Split.

data Array
    = Split Expression
    | Glob  Expression
    | SplitGlob Expression
    | Field Expression
    | ConcatA [Array]
    deriving Show

data Redirection = Redirection Int AST.RedirectionOp Expression
    deriving Show

----------------------------------------
-- Word translation

splitGlobbable :: Expression -> Expression
splitGlobbable = Globbable . Splittable

translateParSubst :: AST.ParSubstExpr -> Expression
translateParSubst (AST.ParSubstExpr par _modifier) =
    -- todo: apply modifier
    Parameter par

translateWord :: AST.Word -> Array
translateWord = SplitGlob . ConcatE . map (translateWordPartCtx False)
  where
    -- translateWordPartCtx isQuoted wordPart
    translateWordPartCtx True  (AST.Bare s) = Const s
    translateWordPartCtx False (AST.Bare s) = Globbable $ Const s

    translateWordPartCtx _ (AST.SQuoted s) = Const s

    translateWordPartCtx _ (AST.Escaped c) = Const [c]

    translateWordPartCtx _ (AST.DQuoted parts) =
        ConcatE $ map (translateWordPartCtx True) parts

    -- fixme: modifier
    translateWordPartCtx isQuoted  (AST.ParSubst e _) =
        (if isQuoted then id else splitGlobbable) $ translateParSubst e

translateWordNoSplitGlob :: AST.Word -> Expression
translateWordNoSplitGlob = ConcatE . map translateWordNoSplitGlobPart
  where
    translateWordNoSplitGlobPart (AST.Bare s) = Const s
    translateWordNoSplitGlobPart (AST.SQuoted s) = Const s
    translateWordNoSplitGlobPart (AST.Escaped c) = Const [c]
    translateWordNoSplitGlobPart (AST.DQuoted s) = translateWordNoSplitGlob s
    translateWordNoSplitGlobPart (AST.ParSubst e _) = translateParSubst e

----------------------------------------
-- Command translation

translateAssignment :: AST.Assignment -> Statement
translateAssignment (AST.Assignment name thing) =
    Assignment name $ translateWordNoSplitGlob thing

translateRedirection :: AST.Redirection -> Redirection
translateRedirection (AST.Redirection fd op word) =
    Redirection fd op $ translateWordNoSplitGlob word

translateSimpleCommand :: AST.SimpleCommand -> Statement
translateSimpleCommand (AST.SimpleCommand as rs ws) =
    let assignments  = map translateAssignment as
        redirections = map translateRedirection rs
        args         = ConcatA $ map translateWord ws
        cmd          = Command args
        cmdWithAssignments =
            if null assignments
                then cmd
                -- those are local assignments, so introduce a subshell
                else Subshell $ Sequence $ assignments ++ [cmd]
        cmdWithAssignmentsAndRedirections =
            if null redirections
                then cmdWithAssignments
                else ApplyRedirections redirections cmdWithAssignments
    in cmdWithAssignmentsAndRedirections

translateAndOrList :: AST.AndOrList -> Statement
translateAndOrList (AST.First x) = translatePipeline x
translateAndOrList (AST.And x l) = And (translatePipeline x) (translateAndOrList l)
translateAndOrList (AST.Or x l) = Or  (translatePipeline x) (translateAndOrList l)

translatePipeline :: AST.Pipeline -> Statement
translatePipeline (AST.Pipeline AST.Straight cmds) =
    Pipeline (helper [] cmds)
    where
        helper :: [Statement] -> [AST.Command] -> [Statement]
        helper acc [] = acc
        helper acc ((AST.ComSimple simpleCmd):xs) = helper (acc ++ [translateSimpleCommand simpleCmd]) xs
        helper _   (x:xs) = error $ "helper in translatePipeline: unimplemented for " ++ show x
translatePipeline x = error $ "translatePipeline: unimplemented for " ++ show x

translateCompoundList :: AST.CompoundList -> Statement
translateCompoundList x =
    Sequence (helper [] x)
    where
        helper :: [Statement] -> AST.CompoundList -> [Statement]
        helper acc [] = acc
        helper acc ((list, mode):xs) =
            if mode == AST.Seq
              then helper (acc ++ [cmd]) xs
              else helper (acc ++ [Subshell cmd]) xs
            where cmd = Sequence [translateAndOrList list]

----------------------------------------
-- Simplifications of IR tree

-- todo: replace with attribute grammar, syb-like or something else

simplifyStatement :: Statement -> Statement
simplifyStatement (Command args) =
    Command (simplifyArray args)
simplifyStatement (Assignment name expr) =
    Assignment name $ fst $ simplifyExpression expr
simplifyStatement (Sequence [s]) = simplifyStatement s
simplifyStatement (Sequence ss) = Sequence $ map simplifyStatement ss
simplifyStatement (ApplyRedirections r s) =
    ApplyRedirections (map simplifyRedirection r) (simplifyStatement s)
simplifyStatement (Pipeline [s]) = simplifyStatement s
simplifyStatement (Pipeline ss) = Pipeline $ map simplifyStatement ss
simplifyStatement (Subshell cmd) = Subshell $ simplifyStatement cmd
simplifyStatement (And cmd cmds) = And (simplifyStatement cmd) (simplifyStatement cmds)
simplifyStatement (Or  cmd cmds) = Or  (simplifyStatement cmd) (simplifyStatement cmds)

simplifyArray :: Array -> Array
simplifyArray (Split e) =
    let (e',f) = simplifyExpression e
    in if splittable f
        then Split e'
        else Field e'
simplifyArray (Glob e) =
    let (e',f) = simplifyExpression e
    in if globbable f
        then Glob e'
        else Field e'
simplifyArray (SplitGlob e) =
    let (e',f) = simplifyExpression e
    in case f of
        Fact { splittable = True,  globbable = True  } -> SplitGlob e'
        Fact { splittable = False, globbable = True  } -> Glob e'
        Fact { splittable = True,  globbable = False } -> Split e'
        Fact { splittable = False, globbable = False } -> Field e'
simplifyArray (Field e) =
    Field $ fst $ simplifyExpression e
simplifyArray (ConcatA [a]) = simplifyArray a
simplifyArray (ConcatA as) = ConcatA $ map simplifyArray as

data Fact = Fact
    { globbable  :: Bool
    , splittable :: Bool
    , mayContainPattern :: Bool
    }

allFalse :: Fact
allFalse = Fact
    { globbable = False
    , splittable = False
    , mayContainPattern = False
    }

simplifyExpression :: Expression -> (Expression, Fact)
simplifyExpression x@Parameter{} = (x, allFalse { mayContainPattern = True })
simplifyExpression x@(Const e)     = (x, allFalse { mayContainPattern = pat })
  where
    pat = any (`elem` e) "*?["
simplifyExpression (Splittable e) =
    let (e',f) = simplifyExpression e
    in  (Splittable e', f { splittable = True })
simplifyExpression (Globbable e) =
    let (e',f) = simplifyExpression e
    in if mayContainPattern f
        then (Globbable e', f { globbable = True })
        else (e',           f { globbable = False })
simplifyExpression (ConcatE [e]) = simplifyExpression e
simplifyExpression x@(ConcatE es) =
    let (es', fs) = unzip $ map simplifyExpression es
        f' = Fact
            { splittable = any splittable fs
            , globbable = any globbable fs
            , mayContainPattern = any mayContainPattern fs}
    in  (ConcatE es', f')

simplifyRedirection :: Redirection -> Redirection
simplifyRedirection (Redirection fd op expr) =
    Redirection fd op $ fst $ simplifyExpression expr

-- Array is plain if we can translate it to a list of
-- Expressions (in particular, we can statically determine its length)
isArrayPlain :: Array -> Maybe [Expression]
isArrayPlain (ConcatA ars) = fmap concat $ mapM isArrayPlain ars
isArrayPlain (Field expr) = Just [expr] -- fmap (:[]) $ isExprConstant expr
isArrayPlain _ = Nothing

isArrayConstant :: Array -> Maybe [String]
isArrayConstant = mapM isExprConstant <=< isArrayPlain

isExprConstant :: Expression -> Maybe String
isExprConstant (ConcatE exprs) = fmap concat $ mapM isExprConstant exprs
isExprConstant (Const s) = Just s
isExprConstant _ = Nothing
