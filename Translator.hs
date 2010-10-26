
module Translator where

import AST
import IR

import Control.Monad.State
import qualified Data.Map as M
import Data.List

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
    return [IR.Const (UserVariable name i) value]

-- the result of optimizeWord is a word with the following properties:
-- 1. there are no SQuoted or Escaped word parts
-- 2. each DQuoted string contains exactly one word part and that part
-- is a substitution (command, parameter or arithmetic)
-- 3. there are no consequtive Bare parts
optimizeWord :: Word -> Word
optimizeWord = concatBares . constToBare . flattenWord

flattenWord :: Word -> Word
flattenWord = concatMap f
  where
    f (DQuoted ws) = map (\x -> DQuoted [x]) ws
    f x = [x]

constToBare :: Word -> Word
constToBare = map f
  where
    f (DQuoted [x@Bare{}]) = x
    f (SQuoted s) = Bare s
    f (Escaped c) = Bare [c]
    f x = x

concatBares :: Word -> Word
concatBares [] = []
concatBares [x] = [x]
concatBares (Bare s1 : Bare s2 : rest) = Bare (s1 ++ s2) : concatBares rest
concatBares (a:rest) = a:concatBares rest

translateSimpleCommand :: SimpleCommand -> UniqueM [Instruction]
translateSimpleCommand (SimpleCommand ax [] [])
    = liftM concat (mapM translateAssignment ax)

translateParameter :: Parameter -> UniqueM Variable
translateParameter (Var name) = do
  i <- newVarVersion name
  return (UserVariable name i)

translateParSubstExpr :: ParSubstExpr -> UniqueM (Variable, [Instruction])
translateParSubstExpr (ParSubstExpr parametr NoModifier) = do
  var <- translateParameter parametr
  return (var, [])

makeConstInstruction :: String -> UniqueM (Variable, [Instruction])
makeConstInstruction s = do
  i <- newUniqueNumber
  let var = InternalVariable (InternalVariableUnique i)
  return (var, [Const var s])

translateWordPart :: WordPart -> UniqueM (Variable, [Instruction])
translateWordPart (Bare s) = makeConstInstruction s
translateWordPart (SQuoted s) = makeConstInstruction s
translateWordPart (Escaped c) = makeConstInstruction [c]
translateWordPart (DQuoted ws) = translateWord ws
translateWordPart (ParSubst e _) = translateParSubstExpr e

translateWord :: Word -> UniqueM (Variable, [Instruction])
translateWord [] = makeConstInstruction ""
translateWord [a] = translateWordPart a
translateWord (w:ws) = do
  (head_v, head_instr) <- translateWordPart w
  (rest_v, rest_instr) <- translateWord ws
  i <- newUniqueNumber
  let v = InternalVariable (InternalVariableUnique i)
  return (v, (head_instr ++ rest_instr ++ [Concat v head_v rest_v]))
