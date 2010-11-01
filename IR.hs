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
