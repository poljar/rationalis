{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Rules
    ( Rule(..)
    , Rules
    , Pattern(..)
    , Objects
    , Arguments
    , MatchVerbs(..)
    , Action(..)
    , SingleWordObject(..)
    , TwoWordObject(..)
    , Adjective(..)
    , Noun(..)
    , ActionVerbs(..)
    , parseRules
    , parseRule
    , parseName
    , singleWordObject
    , twoWordObject
    , objects
    , patternVerbs
    , actionVerbs
    , parsePatternLine
    , parseActionLine
    , parseRulesFile
    ) where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Char
import Text.Megaparsec
#if MIN_VERSION_megaparsec(6,0,0)
import Data.Void (Void)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String
#else
import qualified Text.Megaparsec.Lexer as L

type Parser = Parsec Dec String
#endif
type Rules = [Rule]

data Rule =
    Rule Header
         Patterns
         Actions
    deriving (Show, Eq)

type Header = String

type Argument = String

type Arguments = [Argument]

-- TODO the arguments could be a compiled regex, so we can type check regex
-- strings or they could also be a date and float for date and amount
-- comparison
type Patterns = [Pattern]

data Pattern =
    Pattern Objects
            MatchVerbs
            [Argument]
    deriving (Show, Eq)

type Actions = [Action]

data Action =
    Action ActionVerbs
           Objects
           Argument
    deriving (Show, Eq)

data SingleWordObject
    = Description
    | Comment
    deriving (Show, Eq)

data TwoWordObject =
    TwoWordObject Adjective
                  Noun
    deriving (Show, Eq)

data Adjective
    = Payer
    | Payee
    deriving (Show, Eq)

data Noun
    = Account
    | Currency
    deriving (Show, Eq)

type Objects = Either SingleWordObject TwoWordObject

instance Read Adjective where
    readsPrec _ str
        | str == "payer" = [(Payer, "")]
        | str == "payee" = [(Payee, "")]
        | otherwise = []

instance Read Noun where
    readsPrec _ str
        | str == "account" = [(Account, "")]
        | str == "currency" = [(Currency, "")]
        | otherwise = []

instance Read SingleWordObject where
    readsPrec _ str
        | str == "comment" = [(Comment, "")]
        | str == "description" = [(Description, "")]
        | otherwise = []

data MatchVerbs
    = Is
    | Matches
    deriving (Show, Eq)

instance Read MatchVerbs where
    readsPrec _ str
        | str == "is" = [(Is, "")]
        | str == "matches" = [(Matches, "")]
        | otherwise = []

data ActionVerbs =
    Set
    deriving (Show, Eq)

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
objects = eitherP singleWordObject twoWordObject

singleWordObject :: Parser SingleWordObject
singleWordObject =
    read <$> (symbol "description" <|> symbol "date" <|> symbol "id")

twoWordObject :: Parser TwoWordObject
twoWordObject = do
    adjective <- read <$> (symbol "payee" <|> symbol "payer")
    noun <-
        read <$> (symbol "account" <|> symbol "currency" <|> symbol "amount")
    return $ TwoWordObject adjective noun

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
    sc
    ruleName <- parseName
    sc
    patternLines <- some parsePatternLine
    sc
    actionLines <- some parseActionLine
    return $ Rule ruleName patternLines actionLines

parseRules :: Parser Rules
parseRules = many parseRule <* eof

parseRulesFile file = parse parseRules file <$> readFile file
