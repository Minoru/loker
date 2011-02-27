{-# LANGUAGE TypeFamilies #-}
module CHelpers where
import C

int = LiteralInt

string = LiteralString

_NULL = LiteralNull

infix 4 !
(!) :: (ExprType i ~ CInt, CExpr i) => ar -> i -> CIndex ar
(!) = CIndex

infix 2 .=
(.=) :: (ExprType lhs ~ ExprType rhs, LValue lhs, CExpr rhs)
     => lhs -> rhs -> CStatement
(.=) = CAssignment
