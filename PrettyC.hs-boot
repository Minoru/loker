module PrettyC where
import {-# SOURCE #-} C
import Text.PrettyPrint
cvar :: CVariable t -> Doc
cindex :: CExpr ar => CIndex ar -> Doc
croutine :: Routine t -> Doc
intLiteral :: Int -> Doc
stringLiteral :: String -> Doc
nullLiteral :: Doc
