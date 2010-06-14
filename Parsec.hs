module Parsec ( module Orig, module Parsec, ask, asks ) where
import Text.Parsec as Orig hiding (char,string,Stream,parse,satisfy,oneOf,noneOf)
import qualified Text.Parsec as Base (char,string,satisfy)
import Control.Monad.Reader
import Control.Applicative hiding (many)

type Stream = String

data RS = RS
    { skipLineContinuation :: Bool
    , insideBackQuotes     :: Bool
    , insideEscapedBackQuotes :: Bool
    }

type Parser = ParsecT Stream () (Reader RS)

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
parse p name s = runReader (runPT p () name s) defaultRS
    where defaultRS = RS { skipLineContinuation = True
                         , insideBackQuotes     = False
                         , insideEscapedBackQuotes = False}

oneOf cs  = try $ satisfy (\c -> elem c cs)
noneOf cs = try $ satisfy (\c -> not (elem c cs))

-- currently the end position is one character after the actual end
-- this will be fixed at some point by rewriting parser combinators
recordPos p =
    (\p1 d p2 -> d (p1,p2)) <$> getPosition <*> p <*> getPosition
