module Language.Cpp.Lex (
      main
    , runLexer
    , ParseError
    ) where


import Data.Char
import Data.List
import Data.Monoid
import Data.Ratio
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple.Curry
import Language.Cpp.Pretty
import Language.Cpp.SyntaxToken
import Numeric
import Text.Parsec hiding (newline)
import Text.Parsec.String


type Lexer = Parser


main :: IO ()
main = do
    str <- getContents
    case runLexer str of
        Left err -> print err
        Right toks -> putStrLn $ pretty (ignoreExt :: () -> [SyntaxToken ()]) toks


runLexer :: (Eq a) => String -> Either ParseError [SyntaxToken a]
runLexer = runParser lexC () ""


lexC :: (Eq a) => Lexer [SyntaxToken a]
lexC = do
    many space
    toks <- many (lexSyntaxToken >>= \ts -> many space >> return ts)
    eof
    return $ negateNumbers toks


negateNumbers :: (Eq a) => [SyntaxToken a] -> [SyntaxToken a]
negateNumbers tokens = case tokens of
    t1 : t2 : t3 : ts -> let
        continue = t1 : (negateNumbers $ t2 : t3 : ts)
        tryNegateAndContinue = case negateNumber t3 of
            Nothing -> continue
            Just t -> t1 : t : negateNumbers ts
        in if t2 == Punctuation (punc "-")
            then case t1 of
                Punctuation {} -> tryNegateAndContinue
                Keyword {} -> tryNegateAndContinue
                _ -> continue
            else continue
    Punctuation p : t : ts -> let
        continue = Punctuation p : (negateNumbers $ t : ts)
        in if p == punc "-"
            then case negateNumber t of
                Nothing -> continue
                Just t' -> [t']
            else continue
    t : ts -> t : negateNumbers ts
    [] -> []


negateNumber :: SyntaxToken a -> Maybe (SyntaxToken a)
negateNumber t = case t of
    Integer n -> Just $ Integer $ negate n
    Floating x -> Just $ Floating $ negate x
    _ -> Nothing


newline :: Lexer ()
newline = do
    c <- oneOf "\r\n"
    case c of 
        '\r' -> optional $ char '\n'
        '\n' -> optional $ char '\r'


lexSyntaxToken :: Lexer (SyntaxToken a)
lexSyntaxToken = parserZero
    <|> lexComment --> const Comment
    <|> lexString --> String
    <|> lexChar --> Char
    <|> try lexFloating --> Floating
    <|> lexInteger --> Integer
    <|> lexDirective
    <|> lexPunctuation --> Punctuation
    <|> lexKeyword --> Keyword
    <|> lexIdentifier --> Identifier
    where
        infix 2 -->
        p --> f = fmap f p


lexComment :: Lexer ()
lexComment = lexLineComment <|> lexBlockComment


lexLineComment :: Lexer ()
lexLineComment = do
    try $ string "//"
    many $ noneOf "\r\n"
    return ()


lexBlockComment :: Lexer ()
lexBlockComment = do
    try $ string "/*"
    many nonClosing
    string "*/"
    return ()
    where
        nonClosing = do
            future <- lookAhead $ do
                c1 <- anyChar
                c2 <- anyChar
                return [c1, c2]
            if future == "*/"
                then parserZero
                else anyChar


line :: Lexer String
line = many $ noneOf "\r\n"


wholeWord :: String -> Lexer String
wholeWord str = do
    string str
    notFollowedBy $ alphaNum <|> char '_'
    return str


lexDirective :: Lexer (SyntaxToken a)
lexDirective = do
    char '#'
    many space
    res <- lexInclude
        <|> lexDefine
        <|> lexIf
        <|> lexIfdef
        <|> lexIfndef
        <|> lexEndif
    return $ Directive res


lexIf :: Lexer Directive
lexIf = do
    try $ wholeWord "if"
    many space
    code <- line
    return $ If code


lexIfdef :: Lexer Directive
lexIfdef = do
    try $ wholeWord "ifdef"
    many space
    code <- line
    return $ Ifdef code


lexIfndef :: Lexer Directive
lexIfndef = do
    try $ wholeWord "ifndef"
    many space
    code <- line
    return $ Ifndef code


lexEndif :: Lexer Directive
lexEndif = do
    try $ wholeWord "endif"
    return Endif


lexInclude :: Lexer Directive
lexInclude = do
    try $ wholeWord "include"
    many1 space
    path <- lexString <|> lexBracketString
    return $ Include path


lexDefine :: Lexer Directive
lexDefine = do
    try $ wholeWord "define"
    many1 space
    name <- lexIdentifier
    mArgs <- optionMaybe $ do
        char '('
        args <- (many space >> lexIdentifier >>= \i -> many space >> return i) `sepBy` char ','
        char ')'
        return args
    many space
    code <- line
    return $ Define name mArgs code


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


lexFloating :: Lexer Rational
lexFloating = do
    beforeDecimal <- lexBase 10
    char '.'
    afterDecimal <- lexBase 10
    optional $ oneOf "fF"
    let afterDecimalDigits = case reverse $ dropWhile (== '0') $ reverse $ show afterDecimal of
            "" -> "0"
            ds -> ds
        numer = read $ show beforeDecimal ++ afterDecimalDigits
        denom = 10 * genericLength afterDecimalDigits
    return $ numer % denom


lexInteger :: Lexer Integer
lexInteger = do
    next <- lookAhead anyChar
    num <- case next of
        '0' -> do
            next' <- lookAhead $ anyChar >> anyChar
            case next' of
                'x' -> anyChar >> anyChar >> lexBase 16
                _ -> lexBase 8
        _ -> lexBase 10
    many $ oneOf "uUlL"
    return num


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





