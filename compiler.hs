import IR
import Parse
import Parsec
import System.Exit
import System.Environment
import System.Posix.Temp
import System.IO
import System.Process
import System.Directory


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

        helper defs code n (Command x) = "#include \"routines.h\"\n" ++
            "int main(){" ++ genDefs 0 True (Command x) ++
            genCode 0 True (Command x) ++ "}"

        helper _ _ _ _ = ""

        countDefs :: Statement -> Int
        countDefs (Command _) = 1
        countDefs _ = 0

        genDefs :: Int -> Bool -> Statement -> C
        genDefs n return (Command (ConcatA x)) = "const char* cmd" ++ show n ++
            "[] = { " ++ fromConcatA x ++ "NULL };" ++
            (if return then "int retval;" else "")
        genDefs n return (Command x) = "const char* cmd" ++ show n ++
            "[] = { " ++ fromArray x ++ "NULL };" ++
            (if return then "int retval;" else "")
        genDefs n return x = error "genDefs: Nothing is generated for the following IR: " ++ show x

        fromConcatA :: [Array] -> C
        fromConcatA ((Field x):[]) = "\"" ++ fromExpression x ++ "\", "
        fromConcatA ((Field x):xs) = "\"" ++ fromExpression x ++ "\", " ++
            fromConcatA xs
        fromConcatA x = error "fromConcatA: Nothing is generated for the following IR: " ++ show x

        fromArray :: Array -> C
        fromArray (Field x) = "\"" ++ fromExpression x ++ "\", "
        fromArray x = error "fromArray: Nothing is generated for the following IR: " ++ show x

        fromExpression :: Expression -> C
        fromExpression (Const x) = x
        fromExpression x = error "fromExpression: Nothing is generated for the following IR: " ++ show x

        genCode :: Int -> Bool -> Statement -> C
        genCode n return (Command (ConcatA x)) = (if return then "retval="
            else "") ++ "exec_command (cmd" ++ show n ++ ");" ++
            (if return then "return retval;" else "")
        genCode n return (Command x) = (if return then "retval=" else "") ++
            "exec_command (cmd" ++ show n ++ ");" ++
            (if return then "return retval;" else "")
        genCode n return x = error "genCode: Nothing is generated for the following IR: " ++ show x

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
    (filename, handle) <- openTempFile "." "prog.c"
    hPutStr handle (genC ir)
    hClose handle

    rawSystem "gcc" ["-Wall", "-c", "routines.c", "-o", "routines.o"]
    rawSystem "gcc" ["-Wall", "routines.o", filename]
    removeFile filename
