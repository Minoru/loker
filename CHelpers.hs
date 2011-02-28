{-# LANGUAGE TypeFamilies #-}
module CHelpers where
import C

infix 4 !
(!) :: (ExprType i ~ CInt, CExpr i) => ar -> i -> CIndex ar
(!) = CIndex

infix 2 .=
(.=) :: (ExprType lhs ~ ExprType rhs, LValue lhs, CExpr rhs)
     => lhs -> rhs -> CStatement
(.=) = CAssignment
