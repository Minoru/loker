
module Translator where

import AST
import IR

translateAssignment (Assignment name [Bare value]) = Const (UserVariable name 0) value

-- simpleCommantToInstructions :: SimpleCommand -> [Instruction]
-- simpleCommantToInstructions (SimpleCommand ax [] []) = parseAssignments ax
--     where 
