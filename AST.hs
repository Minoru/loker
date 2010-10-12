module AST where

import Text.Parsec.Pos

-- We need to redefine SourcePos, since original SourcePos from Parsec does
-- not have Read instance and we need it for testing purposes
data Pos = Pos { posName :: String, posLine :: Int, posColumn :: Int }
    deriving (Eq, Show, Read)
fromSourcePos :: SourcePos -> Pos
fromSourcePos sp = Pos
    { posName = sourceName sp
    , posLine = sourceLine sp
    , posColumn = sourceColumn sp
    }
type L = (Pos, Pos)

-- A parameter can be denoted by a name, a number, or one of the special
-- characters listed in Special Parameters.
--
-- A variable is a parameter denoted by a name.
--
-- A positional parameter is a parameter denoted by the decimal value
-- represented by one or more digits, other than the single digit 0.
type Name = String
data Parameter = Var Name
               | Positional Int
               | Special Char
    deriving (Show,Read,Eq)

data SubstringType = Suffix | Prefix
    deriving (Show,Read,Eq)
data PatternType = Largest | Smallest
    deriving (Show,Read,Eq)
data CheckType = CheckUnset | CheckUnsetAndNull
    deriving (Show,Read,Eq)

data ParModifier = UseDefault       Word CheckType
                 | AssignDefault    Word CheckType
                 | Assert           Word CheckType
                 | UseAlternative   Word CheckType
                 | StringLength
                 | Remove           Word PatternType SubstringType
                 | NoModifier
    deriving (Show,Read,Eq)

data ParSubstExpr = ParSubstExpr Parameter ParModifier
    deriving (Show,Read,Eq)

data WordPart = Bare String
               | SQuoted String
               | DQuoted [WordPart]
               | Escaped Char
               | CommandSubst CompoundList L
               | ParSubst ParSubstExpr L
               | Arith [WordPart]
    deriving (Show,Read,Eq)

type Word = [WordPart]

data Redirection = Redirection Int RedirectionOp Word
    deriving (Show,Read,Eq)

data StripHereDoc = Strip | NoStrip
    deriving (Show,Read,Eq)
data Clobber = Clobber | NoClobber
    deriving (Show,Read,Eq)
data RedirectionOp = RedirectInput
                   | RedirectOutput Clobber
                   | AppendOutput
                   | HereDoc StripHereDoc Int
                   | DupInput
                   | DupOutput
                   | ReadWrite
    deriving (Show,Read,Eq)

data Assignment = Assignment Name Word
    deriving (Show,Read,Eq)

data Command = ComSimple SimpleCommand
             | ComCompound CompoundCommand [Redirection]
             | ComFunction FunctionDefinition
    deriving (Show,Read,Eq)
data FunctionDefinition =
	FunctionDefinition Name CompoundCommand [Redirection]
    deriving (Show,Read,Eq)
data SimpleCommand = SimpleCommand [Assignment] [Redirection] [Word]
    deriving (Show,Read,Eq)
data ForList = ForWords [Word] | ForPositional
    deriving (Show,Read,Eq)
data CompoundCommand = BraceGroup CompoundList
                     | SubShell CompoundList
                     | For Name ForList CompoundList
                     | Case Word [([Word],CompoundList)]
                     | If [(CompoundList,CompoundList)] -- 'if' and 'elif'
                           (Maybe CompoundList) -- optional 'else'
                     | While CompoundList CompoundList
                     | Until CompoundList CompoundList
    deriving (Show,Read,Eq)
data PipelineStatus = Straight | Inverted
    deriving (Show,Read,Eq)
data Pipeline = Pipeline PipelineStatus [Command]
    deriving (Show,Read,Eq)
data AndOrList = First Pipeline
               | And   Pipeline AndOrList
               | Or    Pipeline AndOrList
    deriving (Show,Read,Eq)
data ExecutionMode = Seq | Async
    deriving (Show,Read,Eq)
type CompoundList = [(AndOrList,ExecutionMode)]

