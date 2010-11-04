import IR
import Parse
import Parsec
import System.Exit
import System.Environment
import System.Posix.Temp
import System.IO
import System.Process


type C = String

genC :: Statement -> C
genC x = helper "" "" 0 x
  where helper :: C -> C -> Int -> Statement -> C
        
        helper defs code n (Sequence (x:[])) = helper (defs ++ genDefs n True x)
            (code ++ genCode n True x) (n + countDefs x) (Sequence [])
        helper defs code n (Sequence (x:xs)) = helper (defs ++ genDefs n False x)
            (code ++ genCode n False x) (n + countDefs x) (Sequence xs)
        helper defs code n (Sequence _     ) = "#include \"routines.h\"\n" ++
            "int main(){" ++ defs ++ code ++ "}"

        helper defs code n (Command x xs) = "#include \"routines.h\"\n" ++
            "int main(){" ++ genDefs 0 True (Command x xs) ++
            genCode 0 True (Command x xs) ++ "}"

        helper _ _ _ _ = ""

        countDefs :: Statement -> Int
        countDefs (Command _ _) = 1
        countDefs _ = 0

        genDefs :: Int -> Bool -> Statement -> C
        genDefs n return (Command (ConcatA x) _) = "const char* cmd" ++ show n ++
            "[] = { " ++ fromConcatA x ++ "NULL };" ++
            (if return then "int retval;" else "")
        genDefs n return (Command x _) = "const char* cmd" ++ show n ++
            "[] = { " ++ fromArray x ++ "NULL };" ++
            (if return then "int retval;" else "")
        genDefs n return x = "\n\ngenDefs: Nothing is generated for the following IR: " ++ show x ++ "\n\n"

        fromConcatA :: [Array] -> C
        fromConcatA ((Field x):[]) = "\"" ++ fromExpression x ++ "\", "
        fromConcatA ((Field x):xs) = "\"" ++ fromExpression x ++ "\", " ++
            fromConcatA xs
        fromConcatA x         = "\n\nfromConcatA: Nothing is generated for the following IR: " ++ show x ++ "\n\n"

        fromArray :: Array -> C
        fromArray (Field x) = "\"" ++ fromExpression x ++ "\", "
        fromArray x = "\n\nfromArray: Nothing is generated for the following IR: " ++ show x ++ "\n\n"

        fromExpression :: Expression -> C
        fromExpression (Const x) = x
        fromExpression x         = "\n\nfromExpression: Nothing is generated for the following IR: " ++ show x ++ "\n\n"

        genCode :: Int -> Bool -> Statement -> C
        genCode n return (Command (ConcatA x) _) = (if return then "retval="
            else "") ++ "exec_command (cmd" ++ show n ++ ");" ++
            (if return then "return retval;" else "")
        genCode n return (Command x _) = (if return then "retval=" else "") ++
            "exec_command (cmd" ++ show n ++ ");" ++
            (if return then "return retval;" else "")
        genCode n return x = "\n\ngenCode: Nothing is generated for the following IR: " ++ show x ++ "\n\n"

main = do
    args <- getArgs
    str <- case args of
        f:_ -> readFile f
        [] -> getContents
    let ast = parse simpleCommand "-" str
    case ast of
        Left err -> do print err; exitFailure
        Right ast -> compile $ simplifyStatement $ translateSimpleCommand ast

compile ir = do
    let fn = "prog.c"
    h <- openFile fn WriteMode -- todo: create a proper temp file
    hPutStr h (genC ir)
    hClose h

    rawSystem "gcc" ["-Wall", fn]
