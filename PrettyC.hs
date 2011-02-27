{-# LANGUAGE ScopedTypeVariables #-}
-- C pretty-printing

module PrettyC (printC) where

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

ctype :: forall t . CType t => CVariable t -> Doc
ctype _ = text . typeToString $ (undefined :: t)

cvar :: CVariable t -> Doc
cvar v = text (varDesc v) <> int (varId v)

cdecl :: CDeclaration -> Doc
cdecl (CDeclScalar v) = ctype v <+> cvar v <> semi
cdecl (CDeclConstArrStringNT v elements) =
    text "const char*" <+> cvar v <> text "[]" <+> equals <+>
        braces (strings <> comma <+> nullp)
    where
    strings = commaList $ map (doubleQuotes . text) elements

cdecls :: [CDeclaration] -> Doc
cdecls ds = vcat (map cdecl $ ds) <> semi

cfuncall
    :: Maybe Doc -- variable for the result
    -> String    -- function name
    -> [Doc] -- arguments
    -> Doc
cfuncall mbVar funcName args =
    let var = maybe empty (\v -> v <+> equals) mbVar
    in var <+> text funcName <> parens (commaList args) <> semi

address :: Doc -> Doc
address v = char '&' <> v

croutine :: Routine -> Doc
croutine (RunCommand status argv) =
    cfuncall (Just $ cvar status) "exec_command" [cvar argv]

cstatement :: CStatement -> Doc
cstatement (CallRoutine r) = croutine r

include, includeLocal :: String -> Doc
(include, includeLocal) = (includeSep '<' '>', includeSep '"' '"')
    where
    includeSep lsep rsep header =
        text "#include" <+> char lsep <> text header <> char rsep

creturn :: Doc -> Doc
creturn c = text "return" <+> c <> semi

program :: [CDeclaration] -> CStatement -> CVariable CInt -> Doc
program decls stmt status = vcat
    [ includeLocal "routines.h"
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
