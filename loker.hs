module Main where
import System.Environment
import System.Exit
import Parse
import Parsec

main = do
    args <- getArgs
    s <- if null args 
            then getContents
            else readFile (head args)
    let program_and_state = do
            p <- program
            s <- getState
            return (p,s)
    let res = parse program_and_state (if null args then "" else head args) s
    case res of
        Right s -> do print s; exitWith ExitSuccess
        Left m  -> do print m; exitFailure
