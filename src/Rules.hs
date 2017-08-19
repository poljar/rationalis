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

sc :: Parser ()
sc = L.space (void spaceChar) comments empty
comments = L.skipLineComment "#"

symbol    = L.symbol sc
brackets  = between (symbol "[") (symbol "]")
quotes    = between (symbol "\"") (symbol "\"")

isHeaderChar :: Char -> Bool
isHeaderChar '[' = False
isHeaderChar ']' = False
isHeaderChar c   = isPrint c

headearChar = satisfy isHeaderChar <?> "valid header characters"
header = some headearChar

parseHeader :: Parser String
parseHeader = brackets header

isQuotedChar :: Char -> Bool
isQuotedChar '"' = False
isQuotedChar c   = isPrint c

quotedChar = satisfy isQuotedChar <?> "valid characters for a quoted string"
quotedString = quotes $ some quotedChar

objects      = symbol "description" <|> symbol "currency"
matchVerbs   = symbol "is" <|> symbol "matches"
actionVerbs  = symbol "set"

parseMatchLine :: Parser (String, String, [String])
parseMatchLine = L.lineFold sc $ \sc' -> do
    object <- objects
    sc'
    verb <- matchVerbs
    sc'
    arg <- quotedString `sepBy1` try (L.symbol sc' "or") <* sc
    return (object, verb, arg)

parseActionLine :: Parser String
parseActionLine = do
    verb <- actionVerbs
    sc
    object <- objects
    sc
    arg  <- quotedString
    return object

parseSection = do
    header <- parseHeader
    sc
    matchLines <- some parseMatchLine
    sc
    actionLines <- some parseActionLine
    return header

paseRules = many parseSection
