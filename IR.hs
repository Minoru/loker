module IR where

-- IR stands for Intermediate Representation

data StringValue
    = ValVariable Variable
    | ValExitStatus ExitStatusUnique
      deriving (Show,Read,Eq)

data Variable
    = UserVariable String Int -- ordinary shell variable
    | InternalVariable InternalVariableUnique -- once-assigned variable introduced during translation
      deriving (Show,Read,Eq)

data Array = ArrayUnique Int
      deriving (Show,Read,Eq)

newtype ExitStatusUnique = ExitStatusUnique Int
      deriving (Show,Read,Eq)
newtype InternalVariableUnique = InternalVariableUnique Int
      deriving (Show,Read,Eq)

data Instruction
    = Execute
        Variable -- (external) command name
        Array -- positional arguments (starting from argv[1])
    | SpecialBuiltin
        SpecialBuiltin
        Array -- positional arguments
    | Builtin
        Builtin
        Array -- positional arguments
    | Const
        Variable
        String
    | Concat
        Variable
        Variable
        Variable 
    | If
        ExitStatusUnique
        Instruction
        Instruction  
      deriving (Show,Read,Eq)


data SpecialBuiltin
    = Break | Colon | Continue | Dot | Eval | Exec | Exit | Export
    | Readonly | Return | Set | Shift | Times | Trap | Unset
      deriving (Show,Read,Eq)

data Builtin
    = Alias | Bg | Cd | Command | False | Fc | Fg | Getopts | Jobs
    | Kill | Newgrp | Pwd | Read | True | Umask | Unalias | Wait
      deriving (Show,Read,Eq)
