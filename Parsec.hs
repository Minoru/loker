module Parsec ( module Orig, module Parsec, ask, asks ) where
import Text.Parsec as Orig hiding (char,string,Stream,parse,satisfy,oneOf,noneOf,newline)
import qualified Text.Parsec as Base (char,string,satisfy)
import Control.Monad.Reader
import Control.Applicative hiding (many)
import qualified Data.IntMap as I
import AST

type Stream = String

-- State for Reader Monad
data RS = RS
    { skipLineContinuation :: Bool
    , insideBackQuotes     :: Bool
    , insideEscapedBackQuotes :: Bool
    }

-- State for State Monad
-- This state is used for parsing here-docs
-- When we meet here-doc, we put its delimiter into hereDocDelims list.
-- After each newline we check whether we have unread here-docs, and if so,
-- start reading them
-- After the here-doc is read, we remove its delimeter from hereDocDelims and put
-- the contents of heredoc into the hereDocs list.
data SS = SS
    { hereDocHandles :: [HereDocHandle]
    , hereDocs       :: I.IntMap Word
    , numHereDocs    :: Int
    }
    deriving Show
type HereDocHandle = (String, Int, HereDocQuoted)
data HereDocQuoted = HereDocQuoted | HereDocNotQuoted
    deriving Show

-- put here-docs delimeter into the queue
-- returns unique number by which the contents of here-doc may be accessed later
enqueueHereDoc :: String -> HereDocQuoted -> Parser Int
enqueueHereDoc delim quoted = do
    ss <- getState
    let n = numHereDocs ss
    putState ss { hereDocHandles = (delim,n,quoted) : hereDocHandles ss, numHereDocs = n + 1 }
    return n

rememberHereDoc :: Int -> Word -> Parser ()
rememberHereDoc i w = updateState $ \ss -> ss { hereDocs = I.insert i w $ hereDocs ss }

pendingHereDocs :: Parser [HereDocHandle]
pendingHereDocs = hereDocHandles <$> getState

type Parser = ParsecT Stream SS (Reader RS)

lineConts :: Parser ()
lineConts = do 
    many $ try $ Base.string "\\\n"
    return ()

dontSkipLineConts p = do
    -- skip line conts before, do not skip inside
    lineConts
    local dontSkip p
    where
    dontSkip rs = rs { skipLineContinuation = False }

enterBackQuotes p = do
    local enter p
    where
    enter rs = rs { insideBackQuotes = True }

enterEscapedBackQuotes p = do
    local enter p
    where
    enter rs = rs { insideEscapedBackQuotes = True }

-- if skipLineContinuation is True, line continuation will be skipped /before/
-- the char
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = try $ do
    skiplc <- asks skipLineContinuation
    if skiplc
        then do lineConts; Base.satisfy f
        else Base.satisfy f

-- if skipLineContinuation is True, line continuation will be skipped /before/
-- the char
char :: Char -> Parser Char
char x = try $ do
    skiplc <- asks skipLineContinuation
    if skiplc
        then do lineConts; Base.char x
        else Base.char x

-- if skipLineContinuation is True, line continuation will be skipped before and
-- inside the string
string :: String -> Parser String
string s = try $ do
    sequence $ map char s
    return s

parse :: Parser a -> SourceName -> Stream -> Either ParseError a
parse p name s = runReader (runPT p emptySS name s) defaultRS
    where defaultRS = RS { skipLineContinuation = True
                         , insideBackQuotes     = False
                         , insideEscapedBackQuotes = False}
          emptySS = SS { hereDocs = I.empty, hereDocHandles = [], numHereDocs = 0 }

oneOf cs  = try $ satisfy (\c -> elem c cs)
noneOf cs = try $ satisfy (\c -> not (elem c cs))

-- currently the end position is one character after the actual end
-- this will be fixed at some point by rewriting parser combinators
recordPos p =
    (\p1 d p2 -> d (p1,p2)) <$> getPosition <*> p <*> getPosition
