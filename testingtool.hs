{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
import Prelude hiding (catch)
import Control.Applicative
import Control.Monad
import Control.Exception
import System.Environment
import System.IO
import System.Exit
import System.Directory
import Data.List
import Data.Maybe
import AST
import Parsec
import Parse

-- for test with name "name":
-- tests/name.sh is test input
-- tests/name.sh.golden is expected output
data Test = forall a. (Show a, Read a, Eq a) =>
    T { testParser :: Parser a, testName :: String }

withState :: Parser a -> Parser (a, SS)
withState p = (,) <$> p <*> getState

-- the tests
tests :: [Test]
tests = [T command "functions"]


a </> b = a ++ "/" ++ b
a <.> b = a ++ "." ++ b

data TestResult a = NoTest
                  | NoGolden
                  | FailParseTest
                  | FailParseGolden
                  | DifferentResults
                  | OK a
    deriving Show
data TestResultIO a = TestResultIO { runtrt :: IO (TestResult a) }
instance Monad TestResultIO where
    return x = TestResultIO $ return $ OK x
    a >>= f = TestResultIO $ do
        x <- runtrt a
        case x of
            OK y -> runtrt $ f y
            NoTest -> return NoTest
            NoGolden -> return NoGolden
            FailParseTest -> return FailParseTest
            FailParseGolden -> return FailParseGolden
            DifferentResults -> return DifferentResults

onError :: IO a -> TestResult a -> TestResultIO a
onError a r = TestResultIO $ (fmap OK $! a) `catch` (\(e :: SomeException) -> return r)

test :: Test -> IO (TestResult ())
test (T p name) = runtrt $ do
    let inpf  = "tests" </> name <.> "sh"
        outpf = "tests" </> name <.> "sh" <.> "golden"
    inp <- readFile inpf `onError` NoTest
    outString <- readFile outpf `onError` NoGolden
    outp <- evaluate (read outString) `onError` FailParseGolden
    TestResultIO . return $ case parse p inpf inp of
            Right r -> if r == outp then OK () else DifferentResults
            Left _ -> FailParseTest

updateTest :: Test -> IO ()
updateTest (T p name) = do
    let inpf  = "tests" </> name <.> "sh"
        outpf = "tests" </> name <.> "sh" <.> "golden"
    inp <- readFile inpf
    case parse p inpf inp of
        Left err -> do 
            hPutStrLn stderr $ "WARNING: test " ++ name ++ " failed to parse:"
            hPutStrLn stderr (show err)
        Right r -> do
            writeFile outpf $ show r ++ "\n"

-- runs all the tests in the test suite
-- todo: print more information
runTests = do
    results <- forM tests $ \t -> do
        r <- test t
        case r of
            OK _ -> return True
            e -> do
                putStrLn $ "test '" ++ testName t ++ "' failed: " ++ show e
                return False
    putStrLn $ "Successfully run " ++ show (length $ filter id results)
        ++ " out of " ++ show (length tests) ++ " tests"

-- lists all the tests in the test suite
listTests = putStr $ unlines $ map testName tests

-- updates the tests in the test suite
updateAllTests = mapM_ updateTest tests
updateTests names = forM_ names $ \n ->
    case find ((n ==) .testName) tests of
        Just t -> updateTest t
        Nothing -> hPutStrLn stderr $ "WARNING: test "++n++" not found"

-- finds new tests in the "tests/" directory
findNewTests = do
    files <- getDirectoryContents "tests"
    let suffix = ".sh"
        testNames = catMaybes $ flip map files $ \f ->
            let (rsuffix,rprefix) = splitAt (length suffix) $ reverse f
            in if rsuffix == reverse suffix
                then Just $ reverse $ rprefix
                else Nothing
    putStr $ unlines $ testNames \\ map testName tests
    

-- prints usage information
usage = hPutStr stderr $ unlines
    [ "USAGE: "
    , "testingtool --run"
    , "testingtool --help"
    , "testingtool --list"
    , "testingtool --update test ..."
    , "testingtool --update-all-tests"
    , "testingtool --find-new-tests"
    ]

main = do
    args <- getArgs
    case args of
        [] -> runTests
        ["--run"] -> runTests
        ["--help"] -> usage
        ["--list"] -> listTests
        "--update":tests -> updateTests tests
        ["--update-all-tests"] -> updateAllTests
        ["--find-new-tests"] -> findNewTests
        _ -> do usage; exitFailure
