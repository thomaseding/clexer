{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SyntaxToken (
      Identifier
    , Code
    , SyntaxToken(..)
    , Punctuation
    , punc
    , unpunc
    , puncs
    , Keyword
    , kw
    , keywords
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
    | Comment
    deriving (Show, Eq, Ord)


newtype Punctuation = Punc String
    deriving (Show, Eq, Ord)


punc :: String -> Punctuation
punc = Punc


unpunc :: Punctuation -> String
unpunc (Punc s) = s


puncs :: [Punctuation]
puncs = map punc $ words "~ ! % ^ & * ( ) - + = ? / . , ; : [ ] { } | < > << >> ++ -- == ~= != %= ^= &= *= <= >= :: -> && ||"


newtype Keyword = Kw String
    deriving (Show, Eq, Ord)


kw :: String -> Keyword
kw = Kw


keywords :: [Keyword]
keywords = map kw $ words "if"




