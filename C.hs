{-# LANGUAGE GADTs, EmptyDataDecls, ExistentialQuantification,
    ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances,
    TypeFamilies, FlexibleContexts, UndecidableInstances, TypeSynonymInstances #-}
module C
where
import Control.Monad.State
import Control.Arrow
import Text.Printf
import Text.PrettyPrint
import {-# SOURCE #-} qualified PrettyC

data CInt
data CChar

data Array t
data ArrayNT t -- NT stands for null-terminated

type CString = ArrayNT CChar
type CStringArrayNT = ArrayNT CString

-- Since we generate only pointer types, the type can be separated from the
-- identifier.
class CType a where
    typeToString :: a -> String

class CType a => CScalarType a

class (CType (ElemType ar), CType ar) => CArrayType ar
    where type ElemType ar

instance CType CInt
    where typeToString _ = "int"
instance CType CChar
    where typeToString _ = "char"
instance CType t => CType (Array t)
    where typeToString _ = typeToString (undefined :: t) ++ " *";
instance CType t => CType (ArrayNT t)
    where typeToString _ = typeToString (undefined :: t) ++ " *";

instance CScalarType CInt
instance CScalarType CChar

instance CType t => CArrayType (Array   t)
    where type ElemType (Array   t) = t
instance CType t => CArrayType (ArrayNT t)
    where type ElemType (ArrayNT t) = t

data CVariable t = CVariable
    { varId :: Int
    , varDesc :: String
    }
    deriving Show

class CType (ExprType e) => CExpr e where
    type ExprType e

    -- Since this is an open type (class), we need some way to print the
    -- values
    printExpr :: e -> Doc
class CExpr e => LValue e

instance CType t => CExpr (CVariable t)
    where
    type ExprType (CVariable t) = t
    printExpr = PrettyC.cvar
instance CType t => CExpr (Routine t)
    where
    type ExprType (Routine t) = t
    printExpr = PrettyC.croutine
instance (CExpr ar, CArrayType (ExprType ar)) => CExpr (CIndex ar)
    where
    type ExprType (CIndex ar) = ElemType (ExprType ar)
    printExpr = PrettyC.cindex
instance CExpr String -- string literal
    where
    type ExprType String = CString
    printExpr = PrettyC.stringLiteral
instance CExpr Int -- integer literal
    where
    type ExprType Int = CInt
    printExpr = PrettyC.intLiteral
instance CExpr NULL
    where
    type ExprType NULL = CString -- do we need NULL for anything else?
    printExpr _ = PrettyC.nullLiteral

instance CType t => LValue (CVariable t)
instance (LValue e, CArrayType (ExprType e)) => LValue (CIndex e)

data CIndex ar = forall i . (CExpr i, ExprType i ~ CInt) => CIndex ar i

data NULL = NULL

data CDeclaration = forall t . CType t => CDeclaration (CVariable t)

data CStatement
    = forall e . CExpr e => CExprStatement e
    | CSequence [CStatement]
      -- in the assignment, we do not enforce lvalue. Maybe we should.
    | forall lhs rhs . (LValue lhs, CExpr rhs, ExprType lhs ~ ExprType rhs) => CAssignment lhs rhs
    | forall what howMany . (LValue what, CArrayType (ExprType what), CExpr howMany, ExprType howMany ~ CInt) => AllocArray what howMany

data Routine t where
    RunCommand :: forall status argv .
        (CExpr argv, ExprType argv ~ CStringArrayNT) =>
        argv -> Routine CInt
    Strncpy :: forall dest src size .
        (LValue dest, CExpr src, CExpr size,
        ExprType dest ~ CString, ExprType src ~ CString, ExprType size ~ CInt) =>
        dest -> src -> size -> Routine dest
    Strdup :: forall s . (CExpr s, ExprType s ~ CString) =>
        s -> Routine CString
