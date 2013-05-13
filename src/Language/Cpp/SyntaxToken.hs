{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Cpp.SyntaxToken (
      Identifier
    , Code
    , SyntaxToken(..)
    , Directive(..)
    , Punctuation
    , punc
    , unpunc
    , puncs
    , Keyword
    , kw
    , unkw
    , keywords
    ) where


import Data.Char
import Data.List
import Numeric


type Identifier = String
type Code = String


data SyntaxToken a
    = String String
    | Char Char
    | Integer Integer
    | Floating Rational
    | Identifier Identifier
    | Directive Directive
    | Punctuation Punctuation
    | Keyword Keyword
    | Comment
    | Ext a
    deriving (Show, Eq, Ord)


instance Functor SyntaxToken where
    fmap func tok = case tok of
        String s -> String s
        Char c -> Char c
        Integer n -> Integer n
        Floating f -> Floating f
        Identifier i -> Identifier i
        Directive d -> Directive d
        Punctuation p -> Punctuation p
        Keyword k -> Keyword k
        Comment -> Comment
        Ext x -> Ext (func x)


data Directive
    = Include FilePath
    | Define Identifier (Maybe [Identifier]) Code
    | If Code
    | Ifdef Code
    | Ifndef Code
    | Endif
    deriving (Show, Eq, Ord)


newtype Punctuation = Punc String
    deriving (Show, Eq, Ord)


punc :: String -> Punctuation
punc = Punc


unpunc :: Punctuation -> String
unpunc (Punc s) = s


puncs :: [Punctuation]
puncs = map punc $ [
  "{", "}", "[", "]", "(", ")", "<", ">", "<=", ">=",
  "+", "-", "*", "/", "~", "!", "%", "^", "&", "|",
  "<<", ">>", "++", "--",
  "&&", "||", "==", "!=",
  ".", "->", ".*", "->*",
  "=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", "&=", "^=", "|=",
  "?", ":", ",", ";", "::",
  "#", "##",
  "\\"
  ]


newtype Keyword = Kw String
    deriving (Show, Eq, Ord)


kw :: String -> Keyword
kw = Kw


unkw :: Keyword -> String
unkw (Kw s) = s


keywords :: [Keyword]
keywords = map kw $ words $ "alignas alignof and and_eq asm auto bitand bitor bool break case catch char char16_t"
    ++ " char32_t class compl const constexpr const_cast continue decltype default delete do double dynamic_cast"
    ++ " else enum explicit export extern false float for friend goto if inline int long mutable namespace new"
    ++ " noexcept not not_eq nullptr operator or or_eq private protected public register reinterpret_cast"
    ++ " return short signed sizeof static static_assert static_cast struct switch template this thread_local"
    ++ " throw true try typedef typeid typename union unsigned using virtual void volatile wchar_t while xor"
    ++ " xor_eq"









