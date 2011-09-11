import Parse
import Parsec
import IR
import System.Exit

main = do
    str <- getContents
    let ast = parse program "-" str
    case ast of
        Left err -> do print err; exitFailure
        Right ast -> print $ simplifyStatement $ translateCompoundList ast
