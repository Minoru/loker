
module Translator where

import AST
import IR

import Control.Monad.State
import qualified Data.Map as M

type UniqueM = State (M.Map String Int)

newVarVersion :: String -> UniqueM Int
newVarVersion name = do
    m <- get
    let i = M.findWithDefault 0 name m
    put (M.insert name (i+1) m)
    return i

newUniqueNumber :: UniqueM Int  -- number for internal variables
newUniqueNumber = newVarVersion ""

runUniqueM :: UniqueM a -> a
runUniqueM m = evalState m M.empty

translateAssignment :: Assignment -> UniqueM [Instruction]
translateAssignment (Assignment name [Bare value]) = do
    i <- newVarVersion name
    return [IR.Const (UserVariable name 0) value]

translateSimpleCommand :: SimpleCommand -> UniqueM [Instruction]
translateSimpleCommand (SimpleCommand ax [] [])
    = liftM concat (mapM translateAssignment ax)

translateParameter :: Parameter -> UniqueM Variable
translateParameter (Var name) = do
  i <- newVarVersion name
  return (UserVariable name i)

translateParSubstExpr :: ParSubstExpr -> UniqueM ([Instruction], Variable)
translateParSubstExpr (ParSubstExpr parametr NoModifier) = do
  var <- translateParameter parametr
  return ([],var)
