{-# LANGUAGE OverloadedStrings #-}

module Argparse
    ( execArgparse
    , parseMaybe
    , periodParser
    , FetchOptions(..)
    , Options(..)
    , GlobalOptions(..)
    , Command(..)
    ) where

import Lib

import Data.Char
import Data.Semigroup ((<>))
import Data.Time
import Options.Applicative

import qualified Text.ParserCombinators.ReadP as RP

type Account = String

data Command
    = Fetch FetchOptions
    | Convert (Maybe FilePath)
              (Maybe FilePath)
    | Pull Account (Maybe Password)

data FetchOptions =
    FetchOptions FilePath
                 (Maybe String)
                 (Maybe Password)
                 (Maybe Period)
                 (Maybe FilePath)

data GlobalOptions = GlobalOptions
    { confPath :: Maybe FilePath
    , rulePath :: Maybe FilePath
    }

data Options =
    Options GlobalOptions
            Command

digit :: RP.ReadP Char
digit = RP.satisfy isDigit

numbers :: Int -> RP.ReadP Integer
numbers digits = fmap read (RP.count digits digit)

dateSeparator :: RP.ReadP ()
dateSeparator = do
    _ <- RP.char '-' <|> RP.char '/'
    return ()

mdNumberParser :: RP.ReadP Int
mdNumberParser = do
    dateSeparator
    fmap fromInteger $ numbers 2 <|> numbers 1

monthDayParser :: RP.ReadP (Int, Int)
monthDayParser = do
    month <- mdNumberParser
    day <- RP.option 1 mdNumberParser
    if month < 1 || month > 12 || day < 1 || day > 31
        then RP.pfail
        else return (month, day)

numDateParser :: RP.ReadP Day
numDateParser = do
    year <- numbers 4
    rest <- RP.option (1, 1) monthDayParser
    return $ uncurry (fromGregorian year) rest

-- TODO Implement relative dates e.g. last year
-- TODO until date should be optional and set to today by default
periodParser :: RP.ReadP Period
periodParser = do
    RP.skipSpaces
    _ <- RP.optional $ RP.string "from" <|> RP.string "since"
    RP.skipSpaces
    from <- numDateParser
    RP.skipSpaces
    _ <- RP.optional $ RP.string "to" <|> RP.string "until"
    RP.skipSpaces
    to <- numDateParser
    RP.eof
    return (from, to)

parseMaybe :: RP.ReadP a -> String -> Maybe a
parseMaybe parser input =
    case RP.readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result

periodReader :: ReadM Period
periodReader =
    eitherReader $ \arg ->
        case parseMaybe periodParser arg of
            Nothing -> Left ("Cannot parse date: " ++ arg)
            Just period -> Right period

periodOption :: Parser (Maybe Period)
periodOption =
    optional $
    option
        periodReader
        (long "period" <> short 'p' <> metavar "PERIOD" <>
         help "Fetch transactions only for the given time-period.")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opt desc = info (helper <*> opt) $ progDesc desc

parseOutFile :: Parser (Maybe FilePath)
parseOutFile =
    optional $
    strOption $
    long "output-file" <> short 'o' <> metavar "OUTFILE" <>
    help "Output file to use."

parseInFile :: Parser (Maybe FilePath)
parseInFile =
    optional $
    strOption $
    long "input-file" <> short 'i' <> metavar "INFILE" <>
    help "Input file to convert."

parseUser :: Parser (Maybe String)
parseUser =
    optional $
    strOption $
    long "user" <> short 'u' <> metavar "USER" <>
    help "User name to pass to the fetcher."

parsePassword :: Parser (Maybe String)
parsePassword =
    optional $
    strOption $
    long "password" <> short 'P' <> metavar "PASSWORD" <>
    help "Password to pass to the fetcher."

parseAccount :: Parser Account
parseAccount =
    strArgument $ metavar "ACCOUNT" <> help "Account to use for fetching."

parseFetcher :: Parser FilePath
parseFetcher = strArgument $ metavar "FETCHER" <> help "Fetcher to run."

parseFetchOpts :: Parser FetchOptions
parseFetchOpts =
    FetchOptions <$> parseFetcher <*> parseUser <*> parsePassword <*>
    periodOption <*>
    parseOutFile

parseFetch :: Parser Command
parseFetch = Fetch <$> parseFetchOpts

parseConvert :: Parser Command
parseConvert = Convert <$> parseInFile <*> parseOutFile

parsePull :: Parser Command
parsePull = Pull <$> parseAccount <*> parsePassword

parseCommand :: Parser Command
parseCommand =
    subparser $
    command
        "fetch"
        (parseFetch `withInfo` "Fetch transaction data for a configured account") <>
    command
        "convert"
        (parseConvert `withInfo`
         "Convert a file containing transaction data to ledger entries.") <>
    command
        "pull"
        (parsePull `withInfo` "Pull transactions into your ledger directory")

parseOptions :: Parser Options
parseOptions =
    Options <$> (GlobalOptions <$> parseConfPath <*> parseRulePath) <*>
    parseCommand

parseConfPath :: Parser (Maybe FilePath)
parseConfPath =
    optional $
    strOption $
    long "config" <> short 'c' <> metavar "FILENAME" <>
    help "Config file to use"

parseRulePath :: Parser (Maybe FilePath)
parseRulePath =
    optional $
    strOption $
    long "rules" <> short 'r' <> metavar "FILENAME" <> help "Rules file to use"

opts :: ParserInfo Options
opts =
    info
        (parseOptions <**> helper)
        (fullDesc <>
         progDesc
             "Fetch transaction data and convert it to ledger transactions.")

execArgparse :: IO Options
execArgparse = execParser opts
