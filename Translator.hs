
module Translator where

import AST
import IR

import Control.Monad.State

type UniqueM = State Int

newUnique :: UniqueM Int
newUnique = do
    i <- get
    put (i+1)
    return i

runUniqueM :: UniqueM a -> a
runUniqueM m = evalState m 0

translateAssignment :: Assignment -> UniqueM [Instruction]
translateAssignment (Assignment name [Bare value]) = do
    i <- newUnique
    return [Const (UserVariable name i) value]

-- simpleCommantToInstructions :: SimpleCommand -> [Instruction]
-- simpleCommantToInstructions (SimpleCommand ax [] []) = parseAssignments ax
--     where 
