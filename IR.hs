module IR where
import qualified AST

----------------------------------------
-- Intermediate representation

data Statement
    -- recursive nodes
    = Sequence [Statement]
    | Subshell Statement
    | ApplyRedirections [Redirection] Statement
    | Pipeline [Statement]

    -- leaves
    | Command
        Array -- argv, including the command name, argv[0]
        [Assignment] -- assignments local to this command
    | VarAssignment
    deriving Show

data Expression
    = Parameter AST.Parameter
    | ConcatE [Expression]
    | Const String
    | Splittable Expression
    | Globbable Expression
    -- todo: arithmetics
    deriving Show

data Array
    = Split Expression
    | Glob  Expression
    | SplitGlob Expression
    | Field Expression
    | ConcatA [Array]
    deriving Show

data Redirection = Redirection Int AST.RedirectionOp Expression
    deriving Show
data Assignment = Assignment AST.Name Expression
    deriving Show

----------------------------------------
-- Word translation
 
splitGlobbable :: Expression -> Expression
splitGlobbable = Globbable . Splittable

translateParSubst :: AST.ParSubstExpr -> Expression
translateParSubst (AST.ParSubstExpr par modifier_) =
    -- todo: apply modifier
    Parameter par

translateWord :: AST.Word -> Array
translateWord = SplitGlob . ConcatE . map (translateWordPartCtx False)
  where
    -- translateWordPartCtx isQuoted wordPart
    translateWordPartCtx True  (AST.Bare s) = Const s
    translateWordPartCtx False (AST.Bare s) = Globbable $ Const s
    
    translateWordPartCtx _ (AST.SQuoted s) = Const s

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
    translateWordNoSplitGlobPart (AST.DQuoted s) = translateWordNoSplitGlob s
    translateWordNoSplitGlobPart (AST.ParSubst e _) = translateParSubst e

----------------------------------------
-- Command translation

translateAssignment (AST.Assignment name thing) =
    Assignment name $ translateWordNoSplitGlob thing

translateRedirection (AST.Redirection fd op word) =
    Redirection fd op $ translateWordNoSplitGlob word

translateSimpleCommand :: AST.SimpleCommand -> Statement
translateSimpleCommand (AST.SimpleCommand as rs ws) =
    let assignments  = map translateAssignment as
        redirections = map translateRedirection rs
        args         = ConcatA $ map translateWord ws
        cmd          = Command args assignments
    in if null redirections
        then cmd
        else ApplyRedirections redirections cmd

----------------------------------------
-- Simplifications of IR tree

-- todo: replace with attribute grammar, syb-like or something else

simplifyStatement :: Statement -> Statement
simplifyStatement (Command args as) =
    Command (simplifyArray args) (map simplifyAssignment as)
simplifyStatement (Sequence [s]) = simplifyStatement s
simplifyStatement (Sequence ss) = Sequence $ map simplifyStatement ss
simplifyStatement (ApplyRedirections r s) =
    ApplyRedirections (map simplifyRedirection r) (simplifyStatement s)
simplifyStatement (Pipeline [s]) = simplifyStatement s
simplifyStatement (Pipeline ss) = Pipeline $ map simplifyStatement ss

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
    let (e',f) = simplifyExpression e
    in Field e'
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
simplifyExpression x@(Splittable e) =
    let (e',f) = simplifyExpression e
    in  (Splittable e', f { splittable = True })
simplifyExpression x@(Globbable e) =
    let (e',f) = simplifyExpression e
    in if mayContainPattern f
        then (Globbable e', f { globbable = True })
        else (e',           f { globbable = False })
simplifyExpression x@(ConcatE [e]) = simplifyExpression e
simplifyExpression x@(ConcatE es) =
    let (es', fs) = unzip $ map simplifyExpression es
        f' = Fact
            { splittable = any splittable fs
            , globbable = any globbable fs
            , mayContainPattern = any mayContainPattern fs}
    in  (ConcatE es', f')

simplifyAssignment :: Assignment -> Assignment
simplifyAssignment (Assignment name expr) =
    Assignment name $ fst $ simplifyExpression expr

simplifyRedirection :: Redirection -> Redirection
simplifyRedirection (Redirection fd op expr) =
    Redirection fd op $ fst $ simplifyExpression expr
