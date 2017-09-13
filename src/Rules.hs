{-# LANGUAGE OverloadedStrings #-}

module Rules
    ( Rule(..)
    , Rules
    , Pattern(..)
    , Objects(..)
    , MatchVerbs(..)
    , Action(..)
    , ActionVerbs(..)
    , parseRules
    , testrule
    , parseRulesFile
    ) where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.String

import qualified Text.Megaparsec.Lexer as L

testrule :: Rule
testrule =
    Rule
        "nest"
        [Pattern Description Matches ["PBZ ATM*"]]
        [Action Set Description "BANKOMAT"]

type Rules = [Rule]

data Rule =
    Rule Header
         Patterns
         Actions
    deriving (Show)

type Header = String

type Argument = String

-- TODO the arguments could be a compiled regex, so we can type check regex
-- strings
type Patterns = [Pattern]

data Pattern =
    Pattern Objects
            MatchVerbs
            [Argument]
    deriving (Show)

type Actions = [Action]

data Action =
    Action ActionVerbs
           Objects
           Argument
    deriving (Show)

data Objects
    = Description
    | Currency
    deriving (Show)

instance Read Objects where
    readsPrec _ str
        | str == "description" = [(Description, "")]
        | str == "currency" = [(Currency, "")]
        | otherwise = []

data MatchVerbs
    = Is
    | Matches
    deriving (Show)

instance Read MatchVerbs where
    readsPrec _ str
        | str == "is" = [(Is, "")]
        | str == "matches" = [(Matches, "")]
        | otherwise = []

data ActionVerbs =
    Set
    deriving (Show)

instance Read ActionVerbs where
    readsPrec _ str
        | str == "set" = [(Set, "")]
        | otherwise = []

sc :: Parser ()
sc = L.space (void spaceChar) comments empty

comments = L.skipLineComment "#"

symbol = L.symbol sc

brackets = between (symbol "[") (symbol "]")

quotes = between (symbol "\"") (symbol "\"")

isNameChar :: Char -> Bool
isNameChar '[' = False
isNameChar ']' = False
isNameChar c = isPrint c

nameChar = satisfy isNameChar <?> "valid header characters"

name = some nameChar

parseName :: Parser String
parseName = brackets name

isQuotedChar :: Char -> Bool
isQuotedChar '"' = False
isQuotedChar c = isPrint c

quotedChar = satisfy isQuotedChar <?> "valid characters for a quoted string"

quotedString = quotes $ some quotedChar

objects :: Parser Objects
objects = read <$> (symbol "description" <|> symbol "currency")

patternVerbs :: Parser MatchVerbs
patternVerbs = read <$> (symbol "is" <|> symbol "matches")

actionVerbs :: Parser ActionVerbs
actionVerbs = read <$> symbol "set"

parsePatternLine :: Parser Pattern
parsePatternLine =
    L.lineFold sc $ \sc' -> do
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
    arg <- quotedString
    return $ Action verb object arg

parseRule :: Parser Rule
parseRule = do
    name <- parseName
    sc
    patternLines <- some parsePatternLine
    sc
    actionLines <- some parseActionLine
    return $ Rule name patternLines actionLines

parseRules :: Parser Rules
parseRules = many parseRule <* eof

parseRulesFile file = parse parseRules file <$> readFile file
