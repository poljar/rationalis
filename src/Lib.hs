module Lib
    ( parseMaybe
    , periodParser
    , Period
    ) where

import Data.Time
import Data.Time.Clock
import Data.Time.Calendar
import Control.Applicative
import qualified Text.ParserCombinators.ReadP as RP

type Period = (Day, Day)

digit :: RP.ReadP Char
digit =
    RP.satisfy (\char -> char >= '0' && char <= '9')

numbers :: Int -> RP.ReadP Integer
numbers digits =
    fmap read (RP.count digits digit)

dateSeparator :: RP.ReadP ()
dateSeparator = do
    RP.char '-' <|> RP.char '/'
    return ()

mdNumberParser :: RP.ReadP Int
mdNumberParser = do
    dateSeparator
    fmap fromInteger $ numbers 2 <|> numbers 1

monthDayParser :: RP.ReadP (Int, Int)
monthDayParser = do
    month <- mdNumberParser
    day   <- RP.option 1 mdNumberParser
    if month < 1 || month > 12 || day < 1 || day > 31 then
        RP.pfail
    else
        return (month, day)

numDateParser :: RP.ReadP Day
numDateParser = do
    year <- numbers 4
    rest <- RP.option (1, 1) monthDayParser
    return $ fromGregorian year (fst rest) (snd rest)


-- TODO Implement relative dates e.g. last year
-- TODO until date should be optional and set to today by default
periodParser :: RP.ReadP Period
periodParser = do
    RP.skipSpaces
    _    <- RP.optional $ RP.string "from" <|> RP.string "since"
    RP.skipSpaces
    since <- numDateParser
    RP.skipSpaces
    _    <- RP.optional $ RP.string "to" <|> RP.string "until"
    RP.skipSpaces
    until <- numDateParser
    RP.eof
    return (since, until)

parseMaybe :: RP.ReadP a -> String -> Maybe a
parseMaybe parser input =
    case RP.readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result
