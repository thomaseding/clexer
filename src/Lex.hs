module Lex (
    ) where


import Data.Char
import Data.List
import Data.Tuple.Curry
import Numeric
import SyntaxToken
import Text.Parsec
import Text.Parsec.String


type Lexer = Parser


test :: Lexer a -> String -> Either ParseError a
test p = runParser p () ""


lexSyntaxToken :: Lexer SyntaxToken
lexSyntaxToken = parserZero
    <|> lexString --> String
    <|> lexChar --> Char
    <|> lexInteger --> Integer
    <|> lexFloating --> Floating
    <|> lexIdentifier --> Identifier
    <|> lexInclude --> Include
    <|> lexDefine --> uncurry3 Define
    <|> lexPunctuation --> Punctuation
    <|> lexKeyword --> Keyword
    where
        infix 2 -->
        p --> f = fmap f p


lexInclude :: Lexer FilePath
lexInclude = parserZero


lexDefine :: Lexer (Identifier, (Maybe [Identifier]), Code)
lexDefine = parserZero


lexPunctuation :: Lexer Punctuation
lexPunctuation = parserZero


lexKeyword :: Lexer Keyword
lexKeyword = parserZero


lexFloating :: Lexer Rational
lexFloating = parserZero


lexInteger :: Lexer Integer
lexInteger = do
    sign <- flip fmap (optionMaybe $ char '-') $ maybe id $ const negate
    next <- lookAhead anyChar
    num <- case next of
        '0' -> do
            anyChar
            (eof >> return 0) <|> do
                next' <- lookAhead anyChar
                case next' of
                    'x' -> anyChar >> lexBase 16
                    _ -> lexBase 8
        _ -> lexBase 10
    return $ sign num


lexIdentifier :: Lexer Identifier
lexIdentifier = do
    first <- letter <|> char '_'
    rest <- many1 $ alphaNum <|> char '_'
    return $ first : rest


lexRawChar :: Lexer Char
lexRawChar = lexEscapedChar <|> satisfy simple
    where
        special = (`elem` "\\\"'")
        simple c = ' ' <= c && c <= '~' && not (special c)


lexEscapedChar :: Lexer Char
lexEscapedChar = do
    char '\\'
    escapeSymbol <- lookAhead anyChar
    mEscapedChar <- case escapeSymbol of
        '\''-> yield '\''
        '"' -> yield '"'
        '?' -> yield '?'
        '\\'-> yield '\\'
        'a' -> yield '\a'
        'b' -> yield '\b'
        'f' -> yield '\f'
        'n' -> yield '\n'
        'r' -> yield '\r'
        't' -> yield '\t'
        'v' -> yield '\v'
        '0' -> yieldOct
        '1' -> yieldOct
        '2' -> yieldOct
        '3' -> yieldOct
        '4' -> yieldOct
        '5' -> yieldOct
        '6' -> yieldOct
        '7' -> yieldOct
        'x' -> yieldHex
        _ -> return Nothing
    case mEscapedChar of
        Just c -> return c
        Nothing -> parserZero <?> "escape sequence"
    where
        yield c = anyChar >> return (Just c)
        yieldOct = fmap Just lexOctChar
        yieldHex = fmap Just lexHexChar


lexOctChar :: Lexer Char
lexOctChar = fmap (chr . fromIntegral) $ lexBase 8


lexHexChar :: Lexer Char
lexHexChar = char 'x' >> (fmap (chr . fromIntegral) $ lexBase 16)


type Base = Int


lexBase :: Base -> Lexer Integer
lexBase base = do
    ds <- many1 $ satisfy isBaseDigit
    case readInt (fromIntegral base) isBaseDigit toInteger ds of
        [(n, "")] -> return n
        _ -> parserZero
    where
        possibleDigits = take base $ ['0' .. '9'] ++ ['a' .. 'z']
        isBaseDigit c = toLower c `elem` possibleDigits
        toInteger c = maybe (error errorMsg) id $ toLower c `elemIndex` possibleDigits
            where
                errorMsg = "lexBase"


lexString :: Lexer String
lexString = do
    char '"'
    str <- many (lexRawChar <|> char '\'')
    char '"'
    return str


lexChar :: Lexer Char
lexChar = do
    char '\''
    c <- lexRawChar <|> char '"'
    char '\''
    return c








