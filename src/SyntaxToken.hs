module SyntaxToken (
      Identifier
    , Code
    , SyntaxToken(..)
    , Punctuation
    , punc
    , Keyword
    , kw
    ) where


type Identifier = String
type Code = String


data SyntaxToken
    = String String
    | Char Char
    | Integer Integer
    | Floating Rational
    | Identifier Identifier
    | Include FilePath
    | Define Identifier (Maybe [Identifier]) Code
    | Punctuation Punctuation
    | Keyword Keyword
    deriving (Show, Eq, Ord)


newtype Punctuation = Punc String
    deriving (Show, Eq, Ord)


punc :: String -> Punctuation
punc = Punc


newtype Keyword = Kw String
    deriving (Show, Eq, Ord)


kw :: String -> Keyword
kw = Kw




