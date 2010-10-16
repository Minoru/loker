module IR where

-- IR stands for Intermediate Representation

data Value
    = Variable String  -- ordinary shell variable
    | Unique   Integer -- once-assigned variable introduced during translation
    | ExitStatus
    | Array


data Instruction
    = Execute
        Value -- (external) command name
        Array -- positional arguments (starting from argv[1])
    | SpecialBuiltin
        SpecialBuiltin
        Array -- positional arguments
    | Builtin
        Builtin
        Array -- positional arguments
    | Assignment

data SpecialBuiltin
    = Break | Colon | Continue | Dot | Eval | Exec | Exit | Export
    | Readonly | Return | Set | Shift | Times | Trap | Unset

data Builtin
    = Alias | Bg | Cd | Command | False | Fc | Fg | Getopts | Jobs
    | Kill | Newgrp | Pwd | Read | True | Umask | Unalias | Wait


