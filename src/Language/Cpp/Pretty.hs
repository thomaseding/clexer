{-# Language FlexibleInstances #-}
{-# Language TypeSynonymInstances #-}

module Language.Cpp.Pretty (
      pretty
    , ignoreExt
    ) where


import Control.Monad.State
import Control.Monad.Writer hiding (tell)
import qualified Control.Monad.Writer as W
import Data.Char
import Data.Function
import Data.List
import Language.Cpp.SyntaxToken
import Numeric


type Pretty = WriterT String (State PrettyState)


data PrettyState = St {
      prev :: SyntaxToken ()
    , spaceAtEnd :: Maybe Char
    }


tell :: String -> Pretty ()
tell "" = return ()
tell s = do
    mLastSpace <- gets spaceAtEnd
    let (s', mLastSpace') = squashSpaces $ maybe s (\c -> c : s) mLastSpace
    W.tell s'
    modify $ \st -> st { spaceAtEnd = mLastSpace' }


tabify :: String -> String
tabify = unlines . flip evalState 0 . mapM tabifyM . lines


tabifyM :: String -> State Int String
tabifyM "" = return ""
tabifyM str = do
    case head str of
        '}' -> modify (subtract 1)
        _ -> return ()
    tabs <- gets $ flip replicate '\t'
    case last str of
        '{' -> modify (+1)
        _ -> return ()
    return $ tabs ++ str


squashSpaces :: String -> (String, Maybe Char)
squashSpaces = flip runState Nothing . squashSpacesM


squashSpacesM :: String -> State (Maybe Char) String
squashSpacesM "" = return ""
squashSpacesM [c] = if isSpace c
    then gets (maybe c (bestSpace c)) >>= put . Just >> return ""
    else return [c]
squashSpacesM (c:c':cs) = if isSpace c
    then if isSpace c'
        then squashSpacesM $ bestSpace c c' : cs
        else fmap (c :) $ squashSpacesM (c':cs)
    else fmap (c :) $ squashSpacesM (c':cs)


prioritizeSpace :: Char -> Int
prioritizeSpace c = maybe maxBound id $ c `elemIndex` "\n\r\t "


bestSpace :: Char -> Char -> Char
bestSpace c = snd . on min (\c -> (prioritizeSpace c, c)) c


ignoreExt :: a -> [SyntaxToken ()]
ignoreExt _ = []


pretty :: (a -> [SyntaxToken ()]) -> [SyntaxToken a] -> String
pretty f = unlines . filter (not . all isSpace) . lines
    . tabify
    . dropWhile isSpace
    . flip evalState st . execWriterT
    . mapM_ prettyToken
    . filter (`notElem` [Comment, Ext ()])
    . concatMap f'
    where
        st = St {
              prev = Comment
            , spaceAtEnd = Just ' '
            }
        f' tok = case tok of
            Ext x -> f x
            _ -> [fmap (const ()) tok]


prettyToken :: SyntaxToken () -> Pretty ()
prettyToken tok = do
    case tok of
        String s -> spacedNormally $ '"' : escape s ++ "\""
        Char c -> spacedNormally $ '\'' : escape c ++ "'"
        Integer n -> spacedNormally $ show n
        Floating x -> spacedNormally $ show (fromRational x :: Double)
        Identifier name -> spacedNormally name
        Directive d -> prettyDirective d
        Punctuation p -> prettyPunc p
        Keyword k -> do
            spacedNormally $ unkw k
            if k `elem` map kw ["for", "if", "return", "switch", "throw", "while"]
                then tell " "
                else return ()
    modify $ \st -> st { prev = tok }
    where
        spacedNormally = spacedLeftBy $ \t -> case t of
            Identifier {} -> True
            Keyword {} -> True
            Punctuation p -> p == punc ")"
            _ -> False


spacedLeftBy :: (SyntaxToken () -> Bool) -> String -> Pretty ()
spacedLeftBy pred msg = do
    prevTok <- gets prev
    if pred prevTok
        then spacedLeft msg
        else tell msg


prettyDirective :: Directive -> Pretty ()
prettyDirective d = do
    tell "#"
    case d of
        Include path -> tell $ "include \"" ++ escape path ++ "\""
        Define name mArgs code -> do
            tell $ "define " ++ name
            case mArgs of
                Nothing -> return ()
                Just args -> tell $ '(' : intercalate ", " args ++ ")"
            tell $ ' ' : code
        If code -> tell $ "if " ++ code
        Ifdef code -> tell $ "ifdef " ++ code
        Ifndef code -> tell $ "ifndef " ++ code
        Endif -> tell "endif"
    tell "\n"


makesNextUnary :: SyntaxToken () -> Bool
makesNextUnary tok = case tok of
    Punctuation p -> p `elem` (map punc $ words ", - + * / ! ^ & ? : | < > = == !=  %= ^= &= *= -= += |= <= >= <<= >>= && || [ ( { ;")
    Keyword k -> k `elem` (map kw $ ["delete", "else", "return", "sizeof", "throw"] ++ ["and", "and_eq", "bitand", "bitor", "not"])
    Directive {} -> True
    _ -> False


prettyPunc :: Punctuation -> Pretty ()
prettyPunc punctuation = do
    let p = unpunc punctuation
    prevTok <- gets prev
    case p of
        "," -> unspacedLeft $ spacedRight p
        ";" -> unspacedLeft $ p ++ "\n"
        "!" -> spacedLeft p
        "%" -> spaced p
        "^" -> spaced p
        "&" -> if makesNextUnary prevTok
            then spacedLeft p
            else spaced p
        "*" -> if makesNextUnary prevTok
            then spacedLeft p
            else spaced p
        "-" -> if makesNextUnary prevTok
            then spacedLeft p
            else spaced p
        "+" -> spaced p
        "/" -> spaced p
        "?" -> spaced p
        ":" -> spaced p
        "|" -> spaced p
        "<" -> spaced p
        ">" -> spaced p
        "=" -> spaced p
        "==" -> spaced p
        "!=" -> spaced p
        "%=" -> spaced p
        "^=" -> spaced p
        "&=" -> spaced p
        "*=" -> spaced p
        "-=" -> spaced p
        "+=" -> spaced p
        "|=" -> spaced p
        "<=" -> spaced p
        ">=" -> spaced p
        "<<=" -> spaced p
        ">>=" -> spaced p
        "<<" -> spaced p
        ">>" -> spaced p
        "&&" -> spaced p
        "||" -> spaced p
        "::" -> tell p
        "->*" -> tell p
        "->" -> tell p
        ".*" -> tell p
        "." -> tell p
        "[" -> tell p
        "]" -> unspacedLeft p
        "(" -> tell p
        ")" -> unspacedLeft p
        "{" -> spacedLeft $ p ++ "\n"
        "}" -> tell $ '\n' : p ++ "\n"


unspacedLeft :: String -> Pretty ()
unspacedLeft s = do
    modify $ \st -> case spaceAtEnd st of
        Nothing -> st
        Just c -> if c `elem` " \t"
            then st { spaceAtEnd = Nothing }
            else st
    tell s

spaced :: String -> Pretty ()
spaced s = spacedLeft s >> tell " "


spacedLeft :: String -> Pretty ()
spacedLeft s = do
    prevTok <- gets prev
    case prevTok of
        Punctuation p -> if unpunc p `elem` ["(", "[", "{", "}"]
            then return ()
            else tell " "
        _ -> tell " "
    tell s


spacedRight :: String -> String
spacedRight s = s ++ " "


class Escape a where
    escape :: a -> String


instance Escape Char where
    escape = flip evalState Nothing . escapeM


instance Escape String where
    escape = concat . flip evalState Nothing . mapM escapeM


escapeM :: Char -> State (Maybe String) String
escapeM c = do
    c' <- case c of
        '\'' -> return "\\'"
        '"'  -> return "\\\""
        '?'  -> do
            prev <- get
            if prev == Just "?"
                then return "\\?"
                else return "?"
        '\\' -> return "\\\\"
        '\a' -> return "\\a"
        '\b' -> return "\\b"
        '\f' -> return "\\f"
        '\n' -> return "\\n"
        '\r' -> return "\\r"
        '\t' -> return "\\t"
        '\v' -> return "\\v"
        _ -> return $ if ' ' <= c && c <= '~'
            then [c]
            else "\\x" ++ showHex (ord c) ""
    put $ Just c'
    return c'



















