module Lex (
      runLexer
    ) where


import Data.Char
import Data.List
import Data.Monoid
import Data.Ratio
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple.Curry
import Numeric
import SyntaxToken
import Text.Parsec hiding (newline)
import Text.Parsec.String


type Lexer = Parser


runLexer :: String -> Either ParseError [SyntaxToken]
runLexer = runParser lexC () ""


lexC :: Lexer [SyntaxToken]
lexC = do
    many space
    toks <- many (lexSyntaxToken >>= \ts -> many space >> return ts)
    eof
    return toks


newline :: Lexer ()
newline = do
    c <- oneOf "\r\n"
    case c of 
        '\r' -> optional $ char '\n'
        '\n' -> optional $ char '\r'


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
lexInclude = do
    char '#'
    many space
    string "include"
    many1 space
    lexString <|> lexBracketString


lexDefine :: Lexer (Identifier, Maybe [Identifier], Code)
lexDefine = do
    char '#'
    many space
    string "define"
    many1 space
    name <- lexIdentifier
    mArgs <- optionMaybe $ do
        char '('
        args <- (many space >> lexIdentifier >>= \i -> many space >> return i) `sepBy` char ','
        char ')'
        return args
    many space
    code <- many $ noneOf "\n\r"
    return (name, mArgs, code)


lexPunctuation :: Lexer Punctuation
lexPunctuation = do
    cs <- lookAhead $ many1 $ oneOf punctuationChars
    let possiblePuncs = reverse $ inits cs
        mPunc = mconcat $ flip map possiblePuncs $ \possPunc -> if punc possPunc `S.member` punctuationSet
            then First $ Just possPunc
            else First Nothing
    case getFirst mPunc of
        Nothing -> parserZero
        Just p -> string p >> return (punc p)
    

punctuationChars :: [Char]
punctuationChars = nub $ concat $ map unpunc puncs


punctuationSet :: Set Punctuation
punctuationSet = S.fromList puncs


lexKeyword :: Lexer Keyword
lexKeyword = do
    ident <- lookAhead lexIdentifier
    if kw ident `S.member` keywordSet
        then string ident >> return (kw ident)
        else parserZero


keywordSet :: Set Keyword
keywordSet = S.fromList keywords


wholeWord :: Lexer a -> Lexer a
wholeWord p = do
    res <- p
    notFollowedBy $ alphaNum <|> char '_'
    return res


lexFloating :: Lexer Rational
lexFloating = do
    sign <- flip fmap (optionMaybe $ char '-') $ maybe id $ const negate
    beforeDecimal <- lexBase 10
    char '.'
    afterDecimal <- lexBase 10
    optional $ oneOf "fF"
    let afterDecimalDigits = case reverse $ dropWhile (== '0') $ reverse $ show afterDecimal of
            "" -> "0"
            ds -> ds
        numer = read $ show beforeDecimal ++ afterDecimalDigits
        denom = 10 * genericLength afterDecimalDigits
    return $ sign $ numer % denom


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
    many $ oneOf "uUlL"
    return $ sign num


lexIdentifier :: Lexer Identifier
lexIdentifier = do
    first <- letter <|> char '_'
    rest <- many $ alphaNum <|> char '_'
    return $ first : rest


lexRawChar :: [Char] -> Lexer Char
lexRawChar extraSpecials = lexEscapedChar <|> satisfy simple
    where
        special = flip elem $ '\\' : extraSpecials
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
    parts <- many1 (lexString' >>= \s -> many space >> return s)
    return $ concat parts


lexString' :: Lexer String
lexString' = do
    char '"'
    str <- many $ lexRawChar "\""
    char '"'
    return str


lexChar :: Lexer Char
lexChar = do
    char '\''
    c <- lexRawChar "'"
    char '\''
    return c


lexBracketString :: Lexer String
lexBracketString = do
    char '<'
    str <- many $ lexRawChar ">"
    char '>'
    return str





