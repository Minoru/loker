{-# LANGUAGE ScopedTypeVariables,GADTs,FlexibleContexts #-}
-- C pretty-printing

module PrettyC (printC,cvar,cliteral,cindex,croutine) where

import Text.PrettyPrint
import Data.List

import C

indent :: Doc -> Doc
indent = nest 4

emptyLine = text ""

nullp :: Doc
nullp = text "NULL"

commaList :: [Doc] -> Doc
commaList = hcat . intersperse (text ", ")

braceBlock :: Doc -> Doc
braceBlock d = lbrace $+$ indent d $+$ rbrace

ctype :: forall e . CExpr e => e -> Doc
ctype _ = text . typeToString $ (undefined :: ExprType e)

celemtype :: forall e . (CExpr e, CArrayType (ExprType e)) => e -> Doc
celemtype _ = text . typeToString $ (undefined :: ElemType (ExprType e))

cvar :: CVariable t -> Doc
cvar v = text (varDesc v) <> int (varId v)

cdecl :: CDeclaration -> Doc
cdecl (CDeclaration v) = ctype v <+> cvar v <> semi

cliteral :: CLiteral t -> Doc
cliteral (LiteralInt i) = int i
cliteral (LiteralString s) = doubleQuotes $ text s
cliteral LiteralNull = text "NULL"

cindex :: CExpr ar => CIndex ar -> Doc
cindex (CIndex ar i) = parens (printExpr ar) <> brackets (printExpr i)

cdecls :: [CDeclaration] -> Doc
cdecls ds = vcat (map cdecl $ ds)

cfuncall
    :: String -- function name
    -> [Doc]  -- arguments
    -> Doc
cfuncall funcName args =
    text funcName <> parens (commaList args)

address :: Doc -> Doc
address v = char '&' <> v

croutine :: Routine t -> Doc
croutine (RunCommand argv) =
    cfuncall "exec_command" [printExpr argv]
croutine (Strncpy dest src n) =
    cfuncall "strncpy" [printExpr dest, printExpr src, printExpr n]
croutine (Strdup s) =
    cfuncall "strdup" [printExpr s]

cstatement :: CStatement -> Doc
cstatement (CExprStatement r) = printExpr r <> semi
cstatement (CSequence ss) = vcat $ map cstatement ss
cstatement (CAssignment lhs rhs) =
    printExpr lhs <+> equals <+> printExpr rhs <> semi
cstatement (AllocArray what howMany) =
    printExpr what <+> equals <+> cfuncall "malloc" [size] <> semi
    where
    size = parens (printExpr howMany) <+> char '*' <+>
        cfuncall "sizeof"  [celemtype what]

include, includeLocal :: String -> Doc
(include, includeLocal) = (includeSep '<' '>', includeSep '"' '"')
    where
    includeSep lsep rsep header =
        text "#include" <+> char lsep <> text header <> char rsep

creturn :: Doc -> Doc
creturn c = text "return" <+> c <> semi

program :: [CDeclaration] -> CStatement -> CVariable CInt -> Doc
program decls stmt status = vcat
    [ include "stdlib.h"
    , include "string.h"
    , includeLocal "routines.h"
    , emptyLine
    , cdecls decls
    , emptyLine
    , main
    ]
    where
    main =
        text "int main()" $+$
        braceBlock
            (cstatement stmt $+$
            emptyLine $+$
            creturn (cvar status))

printC :: ((CVariable CInt, CStatement), [CDeclaration]) -> String
printC ((status, stmt), decls) = render $ program decls stmt status
