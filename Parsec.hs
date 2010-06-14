module Parsec ( module Orig, module Parsec, ask, asks ) where
import Text.Parsec as Orig hiding (char,string,Stream,parse,satisfy,oneOf,noneOf,newline)
import qualified Text.Parsec as Base (char,string,satisfy)
import Control.Monad.Reader
import Control.Applicative hiding (many)

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
    { hereDocDelims  :: [String]
    , hereDocs       :: [Stream]
    , numHereDocs    :: Int
    }

-- returns unique number by which the contents of here-doc may be accessed later
enqueueHereDoc :: String -> Parser Int
enqueueHereDoc delim = do
    ss <- getState
    let n = numHereDocs ss
    putState ss { hereDocDelims = hereDocDelims ss ++ [delim], numHereDocs = n + 1 }
    return n

dequeueHereDoc :: Parser (Maybe String)
dequeueHereDoc = do
    ss <- getState
    case hereDocDelims ss of
        [] -> return Nothing
        d:ds -> do
            putState ss { hereDocDelims = ds }
            return $ Just d

rememberHereDoc :: Stream -> Parser ()
rememberHereDoc str = updateState $ \ss -> ss { hereDocs = hereDocs ss ++ [str] }

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

-- newline needs special treatment because it may be followed by a here-doc
-- this parser needs to be used everywhere where newline is needed, except
-- 'lineConts'
-- todo: more efficient string concatenation
-- fixme: if here-doc delimeter is not quoted, we also need to parse here-doc
-- for expansions
newline = do
    char '\n'
    -- check if we need to parse any here-docs
    tryHereDoc
    return '\n'
    where
    tryHereDoc = maybe (return ()) readHereDoc =<< dequeueHereDoc
    line = (\s n -> s ++ [n]) <$> many (satisfy (/= '\n')) <*> char '\n'
    readHereDoc delim = do
        readHereDoc'
        tryHereDoc
        where
        delim_nl = delim ++ "\n"
        readHereDoc' = do
            l <- line
            if l == delim_nl
                then return ""
                else (l ++) <$> readHereDoc'
    
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
          emptySS = SS { hereDocs = [], hereDocDelims = [], numHereDocs = 0 }

oneOf cs  = try $ satisfy (\c -> elem c cs)
noneOf cs = try $ satisfy (\c -> not (elem c cs))

-- currently the end position is one character after the actual end
-- this will be fixed at some point by rewriting parser combinators
recordPos p =
    (\p1 d p2 -> d (p1,p2)) <$> getPosition <*> p <*> getPosition
