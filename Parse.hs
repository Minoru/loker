module Parse where
import AST
import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative hiding (many,(<|>),optional)
import Parsec hiding (token,tokens)

singleQuoted :: Parser WordPart
singleQuoted = dontSkipLineConts $ do
    squote
    text <- many nonQuote
    squote
    return $ SQuoted text
    where
    squote = char '\''
    nonQuote = satisfy (/= '\'')

bareWord :: String -> Parser WordPart
bareWord terminators = do
    word <- many1 ordinarySymbol
    return $ Bare word
    where
    ordinarySymbol = noneOf terminators

escaped :: Parser WordPart
escaped = try $ do
    char '\\'
    c <- anyChar
    bq <- asks insideBackQuotes
    -- if we are inside backquotes, quoted backquotes have special meaning
    -- (nesting)
    if c == '`' && bq
        then parserFail ""
        else return $ Escaped c

doubleQuoted :: Parser WordPart
doubleQuoted = do
    dQuote
    parts <- many $ escaped <|> bare_word <|> substitution <|> backquoted
    dQuote
    return $ DQuoted parts
    where
    dQuote = char '"'
    escapables = "$`\"\\\n"
    escaped = try $ do
        char '\\'
        Escaped <$> oneOf escapables
    bare_word = do
        w <- many1 ordinary_symbol
        return $ Bare w
        where
        ordinary_symbol = noneOf "$`\\\"" <|>
            do char '\\'; lookAhead (noneOf escapables); return '\\'

backquoted = do
    RS { insideBackQuotes = alreadyInsideBackQuotes
       , insideEscapedBackQuotes = alreadyInsideEscapedBackQuotes }
        <- ask
    if not alreadyInsideBackQuotes
        then enterBackQuotes backQuotes
        else -- if already inside backquotes,
             -- try to parse escaped backquotes
             if not alreadyInsideEscapedBackQuotes
                then enterEscapedBackQuotes escapedBackQuotes
                else parserFail ""
    where
    backQuotes = recordPos $ do
        char '`'
        cmd <- list
        char '`'
        return $ CommandSubst cmd
    escapedBackQuotes = recordPos $ do
        string "\\`"
        cmd <- list
        string "\\`"
        return $ CommandSubst cmd

word :: String -> Bool -> Parser Word
word terminators acceptEmpty = do
    (if acceptEmpty then many else many1) $
        escaped <|> singleQuoted <|> doubleQuoted <|> substitution <|> backquoted <|> bareWord terminators

--- Operators ---

operator :: Parser String
operator = token $ do
    choice $ newline_str : map (try.string) operatorList
    where newline_str = (:[]) <$> newline

theOperator op = try $ do
    op' <- operator
    guard $ op' == op
    return op

-- we don't include \n to the operatorList because \n has its own special parser
operatorList = ["(",")","&&","||",";;","<<",">>","<&",">&","<>","<<-",">|","<",">","&",";","|"]
opFirstLetters = '\n' : (nub . map head) operatorList

--- Comments ---

comment :: Parser ()
comment = do
    char '#'
    many $ satisfy (/= '\n')
    return ()

whiteSpace :: Parser ()
whiteSpace = do many1 $ comment <|> (oneOf " \t" >> return ());
                return ()

--- Substitutions ---
-- Parameter expansion, command substitution or arithmetic expansion
substitution :: Parser WordPart
substitution = recordPos $
    char '$' *>
    (ParSubst <$> parameterSubst <|> CommandSubst <$> commandSubst)

-- A word consisting solely of underscores, digits, and alphabetics from the
-- portable character set. The first character of a name is not a digit. 
name :: Parser Name
name = do
    first <- underscore <|> letter
    rest <- many $ underscore <|> letter <|> digit
    return $ first:rest
    where
    -- portable character set
    letter = satisfy $ \x -> (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z')
    underscore = char '_'

token_name :: Parser Name
token_name = token name

parameterSubst :: Parser ParSubstExpr
parameterSubst = do
    braced <|> unbraced <?> "parameter substitution"
    where
    lbrace = char '{'
    rbrace = char '}'

    braced = between lbrace rbrace $
        try string_length <|> 
        try parameter_check <|> 
        try parameter_substr <|>
        try (flip ParSubstExpr NoModifier <$> parameter)

    parameter = do special <|> positional <|> Var <$> name 

    word_arg = word "}'\"`$\\" True

    parameter_check = do
        par <- parameter
        colon <- optionMaybe $ char ':'
        op <- oneOf "-+=?"
        w <- word_arg
        
        let mod = case op of
             '-' -> UseDefault
             '=' -> AssignDefault
             '+' -> UseAlternative
             '?' -> Assert
        
        let checkType = if isJust colon then CheckUnsetAndNull else CheckUnset

        return $ ParSubstExpr par (mod w checkType)

    parameter_substr = do
        par <- parameter
        op <- choice $ map (try.string) ["%%","%","##","#"]
        w <- word_arg

        let mod = case op of
             "%"  -> Remove w Smallest Suffix
             "%%" -> Remove w Largest Suffix
             "#"  -> Remove w Smallest Prefix
             "##" -> Remove w Largest Prefix
        return $ ParSubstExpr par mod

    string_length = do
        char '#'
        par <- parameter
        return $ ParSubstExpr par StringLength

    unbraced = do
        par <- special -- should be the first to capture 0
           <|> simple_positional
           <|> Var <$> name
        return $ ParSubstExpr par NoModifier
    
    variable = Var <$> name <?> "variable"

    simple_positional = do
        d <- digit
        return $ Positional $ read [d]

    positional = Positional <$> number
    
    special = Special <$> oneOf "@*#?-$!0"

commandSubst :: Parser CompoundList
commandSubst = between (char '(') (char ')') compoundList

--- Tokens ---

token :: Parser a -> Parser a
token p = do optional whiteSpace; x <- p; optional whiteSpace; return x

separated p = do
    optional whiteSpace
    sepEndBy p (optional whiteSpace)

separated1 p = do
    optional whiteSpace
    sepEndBy1 p (optional whiteSpace)

token_word = token $ word ("'\"`$\\\n# " ++ opFirstLetters) False

--- Syntax ---
redirOp :: Parser RedirectionOp
redirOp = do
    op <- choice $ map (try.string) ["<<-",">>","<&",">&","<>","<<",">|","<",">"]
    return $ case op of
        "<"  -> RedirectInput
        ">"  -> RedirectOutput NoClobber
        ">|" -> RedirectOutput Clobber
        "<<" -> HereDoc NoStrip undefined
        "<<-"-> HereDoc Strip   undefined
        ">>" -> AppendOutput
        "<&" -> DupInput
        ">&" -> DupOutput
        "<>" -> ReadWrite

redirection :: Parser Redirection
redirection = do
    mbFd <- optionMaybe number
    op <- redirOp
    -- fixme: if op is HereDoc, we should parse w differently
    w <- case op of
        HereDoc {} -> hereDocDelim
        _ -> token_word

    let fd = case mbFd of
            Just fd -> fd
            Nothing -> case op of
                RedirectOutput _ -> 1
                AppendOutput     -> 1
                DupOutput        -> 1
                RedirectInput    -> 0
                HereDoc _ _      -> 0
                DupInput         -> 0
                ReadWrite        -> 0

    op' <- case op of
        HereDoc strip _ -> do
            i <- enqueueHereDoc (unquote w)
            return $ HereDoc strip i
        _ -> return op

    return $ Redirection fd op' w
    where
    unquote = concatMap unquote1
    unquote1 (Bare s) = s
    unquote1 (SQuoted s) = s
    unquote1 (DQuoted w) = unquote w
    unquote1 (Escaped c) = [c]
    unquote1 x = error $ "Got unexpected thingy in here-doc delimeter: " ++ show x
    hereDocDelim = token $ many1 $
                escaped <|> singleQuoted <|> doubleQuoted <|> bareWord "'\"\\\n# "
    doubleQuoted :: Parser WordPart
    doubleQuoted = do
        dQuote
        parts <- many $ escaped <|> bare_word
        dQuote
        return $ DQuoted parts
        where
        dQuote = char '"'
        escapables = "$`\"\\\n"
        escaped = try $ do
            char '\\'
            Escaped <$> oneOf escapables
        bare_word = do
            w <- many1 ordinary_symbol
            return $ Bare w
            where
            ordinary_symbol = noneOf "\\\"" <|>
                do char '\\'; lookAhead (noneOf escapables); return '\\'


assignment = do
    var <- name
    char '='
    value <- token_word
    return $ Assignment var value

ifNotReserved :: Parser a -> Parser a
ifNotReserved p = try $ do
    r <- optionMaybe reservedWord
    case r of
	Just _ -> parserFail "unexpected reserved word"
	Nothing -> p

simpleCommand = ifNotReserved $ do
    cmd_prefix <- separated (add_assignment <$> (try assignment) <|> add_redirection <$> redirection)
    cmd_word   <- maybeToList <$> optionMaybe (add_word <$> token_word)
    cmd_suffix <- separated (try (add_redirection <$> redirection) <|> add_word <$> token_word)

    let (as,rs,ws) = foldr ($) ([],[],[]) (cmd_prefix ++ cmd_word ++ cmd_suffix)

    case (as,rs,ws) of
        ([],[],[]) -> parserFail "Empty command"
        _ -> return $ SimpleCommand as rs ws

    where
    add_assignment  a (as,rs,ws) = (a:as,rs,ws)
    add_redirection r (as,rs,ws) = (as,r:rs,ws)
    add_word        w (as,rs,ws) = (as,rs,w:ws)

reservedWords = ["!",  "{", "}", "case", "do", "done", "elif", "else", "esac",
                 "fi", "for", "if", "in", "then", "until", "while"]

reservedWord = try $ do
    [Bare x] <- token_word
    guard $ x `elem` reservedWords
    return $ x
theReservedWord w = (<?> "reserved word \"" ++ w ++ "\"") $ try $ do
    w' <- reservedWord
    guard $ w == w'
    return w

linebreak = separated newline
newline_list = separated1 $ newline
sequential_sep = (theOperator ";" >> linebreak) <|> newline_list

pipeline = do
    bang <- do optionMaybe (theReservedWord "!")
    ps <- pipe_sequence
    let status = if isJust bang then Inverted else Straight
    return $ Pipeline status ps
    where
    pipe_sequence = do
        sepBy1 command (do theOperator "|"; linebreak)

functionDefinition :: Parser FunctionDefinition
functionDefinition = ifNotReserved $ do
    fname <- token_name
    string "()"
    linebreak
    (body,redirs) <- compoundCommand
    return $ FunctionDefinition fname body redirs

command :: Parser Command
command = try $ ComFunction <$> functionDefinition
      <|> (uncurry ComCompound) <$> compoundCommand
      <|> ComSimple <$> simpleCommand
      

andOrList = do
    p <- pipeline
    op <- optionMaybe $ theOperator "&&" <|> theOperator "||"
    case op of
        Nothing -> return $ First p
        Just op -> do
            linebreak
            rest <- andOrList
            let opCon = case op of
                 "&&" -> And
                 "||" -> Or
            return $ opCon p rest

list :: Parser CompoundList
list = do
    linebreak
    aol <- andOrList
    mode <- optionMaybe $
        (do theOperator ";"; linebreak; return Seq)   <|>
        (do theOperator "&"; linebreak; return Async) <|>
        (do newline_list;               return Seq)
    -- if there is separator, try to parse list further
    -- if there is no, finish
    case mode of
        Just mode -> do
                rest <- optionMaybe list
                return $ (aol,mode) : (concat.maybeToList) rest
        Nothing -> return [(aol,Seq)]

compoundList = list

compoundCommand :: Parser (CompoundCommand, [Redirection])
compoundCommand = do
    cmd <- choice
        [ braceGroup
        , subShell
        , forClause
        , ifClause
        , whileClause
        , untilClause
        , caseClause
        ]
    redirs <- many redirection
    return (cmd, redirs)

braceGroup = BraceGroup <$> between (theReservedWord "{") (theReservedWord "}") compoundList

subShell = SubShell <$> between (theOperator "(") (theOperator ")") compoundList

doGroup = between (theReservedWord "do") (theReservedWord "done") compoundList

forClause = do
    theReservedWord "for"
    var <- token_name
    linebreak
    words <- optionMaybe wordlist
    do_group <- doGroup
    let list = case words of
         Just ws -> ForWords ws
         Nothing -> ForPositional
    return $ For var list do_group
    where
    wordlist = do
        theReservedWord "in"
        ws <- many token_word
        sequential_sep
        return ws

whileClause = do
    theReservedWord "while"
    l <- compoundList
    cmds <- doGroup
    return $ While l cmds

untilClause = do
    theReservedWord "until"
    l <- compoundList
    cmds <- doGroup
    return $ Until l cmds

ifClause = do
    theReservedWord "if"
    cond <- compoundList
    theReservedWord "then"
    then_part <- compoundList
    elif_parts <- many elif_part
    mb_else_part <- optionMaybe else_part
    theReservedWord "fi"
    return $ If ((cond,then_part):elif_parts) mb_else_part

    where
    elif_part = do
        theReservedWord "elif"
        cond <- compoundList
        theReservedWord "then"
        then_part <- compoundList
        return (cond, then_part)

    else_part = do
        theReservedWord "else"
        compoundList

caseClause = do
    theReservedWord "case"
    w <- token_word
    linebreak
    theReservedWord "in"
    linebreak
    cl <- case_list

    return $ Case w cl
    
    where
    -- case_item:
    --   returns Left () if it parsed "esac",
    --   returns Right (item, True) if it has parsed item with DSEMI
    --   returns Right (item, False) if it has parsed item without DSEMI
    pattern :: Parser [Word]
    pattern = sepBy1 token_word (char '|')

    case_item :: Parser (Either () (([Word],CompoundList), Bool))
    case_item = do
        openParen <- optionMaybe (theOperator "(")
        let esac = case openParen of
              -- if there was opening paren, don't recognise esac as reserved word
              Nothing -> do theReservedWord "esac"; return $ Left ()
              Just _ -> parserFail ""

            item = do
                pat <- pattern
                theOperator ")"
                linebreak
                cl <- optionMaybe compoundList
                dsemi <- optionMaybe (theOperator ";;")
                linebreak
                return $ Right ((pat,concat $ maybeToList cl),isJust dsemi)

        esac <|> item

    case_list = do
        ci <- case_item
        case ci of
            Left _ -> return []
            Right (pat,True) -> (pat:) <$> case_list
            Right (pat,False) -> do theReservedWord "esac"; return [pat]

program = do
    l <- list
    eof
    return l

--- Misc ---

number = do
    n <- many1 digit
    return $ read n
