{-# LANGUAGE TypeFamilies,FlexibleContexts #-}
module CHelpers where
import C
import CodeGenMonad

infix 4 !
(!) :: (ExprType i ~ CInt, CExpr i) => ar -> i -> CIndex ar
(!) = CIndex

-- Lift CStatements to CGM monad
infix 2 .=
(.=) :: (ExprType lhs ~ ExprType rhs, LValue lhs, CExpr rhs)
     => lhs -> rhs -> CGM ()
a .= b = addCode $ CAssignment a b

allocArray :: (LValue what, CArrayType (ExprType what), CExpr howMany, ExprType howMany ~ CInt) =>
    what -> howMany -> CGM ()
allocArray what howMany = addCode $ AllocArray what howMany

expr :: CExpr e => e -> CGM ()
expr e = addCode $ CExprStatement e
