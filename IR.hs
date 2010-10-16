module IR where

-- IR stands for Intermediate Representation

data StringValue
    = ValVariable Variable
    | ValExitStatus ExitStatusUnique

data Variable
    = UserVariable String Int -- ordinary shell variable
    | InternalVariable InternalVariableUnique -- once-assigned variable introduced during translation

data Array = ArrayUnique Int

newtype ExitStatusUnique = ExitStatusUnique Int
newtype InternalVariableUnique = InternalVariableUnique Int

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

data SpecialBuiltin
    = Break | Colon | Continue | Dot | Eval | Exec | Exit | Export
    | Readonly | Return | Set | Shift | Times | Trap | Unset

data Builtin
    = Alias | Bg | Cd | Command | False | Fc | Fg | Getopts | Jobs
    | Kill | Newgrp | Pwd | Read | True | Umask | Unalias | Wait

data Script = [Instruction]
