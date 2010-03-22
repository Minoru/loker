module Main where
import System.Environment
import System.Exit
import Parse
import Parsec (parse)

main = do
    args <- getArgs
    s <- if null args 
            then getContents
            else readFile (head args)
    let res = parse program (if null args then "" else head args) s
    case res of
        Right _ -> do putStrLn "OK"; exitSuccess
        Left m  -> do print m; exitFailure
