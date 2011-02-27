{-# LANGUAGE TupleSections #-}
import IR
import IR2C
import PrettyC
import C
import Parse
import Parsec
import System.Exit
import System.Environment
import System.Posix.Temp
import System.IO
import System.Process
import System.Directory
import Control.Applicative

main = do
    args <- getArgs
    (str, name) <- case args of
        f:_ -> (,f) <$> readFile f
        [] -> (,"-") <$> getContents
    let ast = parse simpleCommand name str
    case ast of
        Left err -> do print err; exitFailure
        Right ast -> compile $ simplifyStatement $ translateSimpleCommand ast

compile ir = do
    (filename, handle) <- openTempFile "." "prog.c"
    hPutStr handle $ printC $ ir2c $ ir
    hClose handle

    rawSystem "gcc" ["-Wall", "-c", "routines.c", "-o", "routines.o"]
    code <- rawSystem "gcc" ["-Wall", "routines.o", filename]
    case code of
        ExitSuccess -> removeFile filename
        _ -> return ()
