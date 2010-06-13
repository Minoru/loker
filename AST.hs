module AST where

import Text.Parsec.Pos (SourcePos)

type L = (SourcePos, SourcePos)

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
    deriving Show

data SubstringType = Suffix | Prefix
    deriving Show
data PatternType = Largest | Smallest
    deriving Show
data CheckType = CheckUnset | CheckUnsetAndNull
    deriving Show

data ParModifier = UseDefault       Word CheckType
                 | AssignDefault    Word CheckType
                 | Assert           Word CheckType
                 | UseAlternative   Word CheckType
                 | StringLength
                 | Remove           Word PatternType SubstringType
                 | NoModifier
    deriving Show

data ParSubstExpr = ParSubstExpr Parameter ParModifier
    deriving Show

data WordPart = Bare String
               | SQuoted String
               | DQuoted [WordPart]
               | Escaped Char
               | CommandSubst CompoundList L
               | ParSubst ParSubstExpr L
               | Arith [WordPart]
    deriving Show

type Word = [WordPart]
data Token = Word [WordPart]
           | Op String
    deriving Show

data Redirection = Redirection Int RedirectionOp Word
    deriving Show

data StripHereDoc = Strip | NoStrip
    deriving Show
data Clobber = Clobber | NoClobber
    deriving Show
data RedirectionOp = RedirectInput
                   | RedirectOutput Clobber
                   | AppendOutput
                   | HereDoc StripHereDoc
                   | DupInput
                   | DupOutput
                   | ReadWrite
    deriving Show

data Assignment = Assignment Name Word
    deriving Show

data Command = ComSimple SimpleCommand
             | ComCompound CompoundCommand [Redirection]
             | ComFunction FunctionDefinition
    deriving Show
data FunctionDefinition =
	FunctionDefinition Name CompoundCommand [Redirection]
    deriving Show
data SimpleCommand = SimpleCommand [Assignment] [Redirection] [Word]
    deriving Show
data ForList = ForWords [Word] | ForPositional
    deriving Show
data CompoundCommand = BraceGroup CompoundList
                     | SubShell CompoundList
                     | For Name ForList CompoundList
                     | Case Word [([Word],CompoundList)]
                     | If [(CompoundList,CompoundList)] -- 'if' and 'elif'
                           (Maybe CompoundList) -- optional 'else'
                     | While CompoundList CompoundList
                     | Until CompoundList CompoundList
    deriving Show
data PipelineStatus = Straight | Inverted
    deriving Show
data Pipeline = Pipeline PipelineStatus [Command]
    deriving Show
data AndOrList = First Pipeline
               | And   Pipeline AndOrList
               | Or    Pipeline AndOrList
    deriving Show
data ExecutionMode = Seq | Async
    deriving Show
type CompoundList = [(AndOrList,ExecutionMode)]

