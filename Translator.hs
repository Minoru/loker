
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
    return [IR.Const (UserVariable name 0) value]

optimizeWord :: Word -> Word    -- concatenate consecutive Bare, SQuoted, Escaped to one Bare string
optimizeWord ws = map concatWordParts groups
    where groups = groupBy (\ x y -> concatenable x && concatenable y) ws

          concatenable :: WordPart -> Bool
          concatenable (Bare x) = True
          concatenable (SQuoted x) = True
          concatenable (Escaped x) = True
          concatenable (DQuoted ws) = and (map concatenable ws)
          concatenable _ = False

          extractString :: WordPart -> String
          extractString (Bare s) = s
          extractString (SQuoted s) = s
          extractString (Escaped c) = [c]
          extractString (DQuoted ws) = extractString (concatWordParts ws)

          concatWordParts :: [WordPart] -> WordPart
          concatWordParts [DQuoted ws] = DQuoted (optimizeWord ws)
          concatWordParts [x] = x
          concatWordParts ws = Bare (concat (map extractString ws))

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
