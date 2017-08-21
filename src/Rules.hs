{-# LANGUAGE OverloadedStrings #-}
module Rules
    where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Void
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.String

import qualified Text.Megaparsec.Lexer as L

data Rule = Rule
    { header  :: String
    , pattern :: [Pattern]
    , action  :: [Action]
    } deriving (Show)

data Pattern     = Pattern
    { matchObject :: Objects
    , matchVerb   :: MatchVerbs
    , arguments   :: [String]
    } deriving (Show)

data Action = Action
    { actionVerb   :: ActionVerbs
    , actionObject :: Objects
    , argument     :: String
    } deriving (Show)

data Objects     = Description | Currency deriving (Show)
instance Read Objects where
    readsPrec _ str = if str == "description" then [(Description, "")]
                      else if  str == "currency" then [(Currency, "")]
                      else []

data MatchVerbs  = Is | Matches deriving (Show)
instance Read MatchVerbs where
    readsPrec _ str = if str == "is" then [(Is, "")]
                      else if  str == "matches" then [(Matches, "")]
                      else []

data ActionVerbs = Set deriving (Show)
instance Read ActionVerbs where
    readsPrec _ str = if str == "set" then [(Set, "")]
                      else []

sc :: Parser ()
sc = L.space (void spaceChar) comments empty
comments = L.skipLineComment "#"

symbol    = L.symbol sc
brackets  = between (symbol "[") (symbol "]")
quotes    = between (symbol "\"") (symbol "\"")

isNameChar :: Char -> Bool
isNameChar '[' = False
isNameChar ']' = False
isNameChar c   = isPrint c

nameChar = satisfy isNameChar <?> "valid header characters"
name = some nameChar

parseName :: Parser String
parseName = brackets name

isQuotedChar :: Char -> Bool
isQuotedChar '"' = False
isQuotedChar c   = isPrint c

quotedChar = satisfy isQuotedChar <?> "valid characters for a quoted string"
quotedString = quotes $ some quotedChar

objects :: Parser Objects
objects = read <$> (symbol "description" <|> symbol "currency")

patternVerbs :: Parser MatchVerbs
patternVerbs   = read <$> (symbol "is" <|> symbol "matches")

actionVerbs :: Parser ActionVerbs
actionVerbs  = read <$> (symbol "set")

parsePatternLine :: Parser Pattern
parsePatternLine = L.lineFold sc $ \sc' -> do
    object <- objects
    sc'
    verb <- patternVerbs
    sc'
    arg <- quotedString `sepBy1` try (L.symbol sc' "or") <* sc
    return $ Pattern object verb arg

parseActionLine :: Parser Action
parseActionLine = do
    verb <- actionVerbs
    sc
    object <- objects
    sc
    arg  <- quotedString
    return $ Action verb object arg

parseRule :: Parser Rule
parseRule = do
    name <- parseName
    sc
    patternLines <- some parsePatternLine
    sc
    actionLines <- some parseActionLine
    return $ Rule name patternLines actionLines

parseRules :: Parser [Rule]
parseRules = many parseRule <* eof
