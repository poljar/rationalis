{-# LANGUAGE OverloadedStrings #-}
module Argparse
    ( execArgparse
    , Options(..)
    , GlobalOptions(..)
    , Command(..)
    )
    where

import Lib

import Data.Time
import Data.Time.Clock
import Data.Time.Calendar
import Data.Maybe
import Data.Time.Format
import Options.Applicative
import Control.Applicative
import Data.Semigroup ((<>))

import qualified Text.ParserCombinators.ReadP as RP

data Command
    = Fetch Period
  | Convert (Maybe FilePath) (Maybe FilePath)

data GlobalOptions = GlobalOptions
    { confPath :: Maybe FilePath
    , rulePath :: Maybe FilePath
    }

data Options = Options
    { globalOpts :: GlobalOptions
    , cmd        :: Command
    }

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


periodReader :: ReadM (Period)
periodReader = eitherReader $ \arg ->
    case parseMaybe periodParser arg of
        Nothing -> Left ("Cannot parse date: " ++ arg)
        Just period -> Right period

periodOption :: Parser Period
periodOption = option periodReader
              ( long "period"
                 <> short 'p'
                 <> metavar "PERIOD"
                 <> help "Fetch transactions only for the given time-period."
              )

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseFetch :: Parser Command
parseFetch = Fetch <$> periodOption

parseConvert :: Parser Command
parseConvert = Convert <$>
    ( optional $ strOption $
       long  "input-file"
    <> short 'i'
    <> metavar "INFILE"
    <> help "Input file to convert."
    )
    <*>
    ( optional $ strOption $
       long  "output-file"
    <> short 'o'
    <> metavar "OUTFILE"
    <> help "Output file to use."
    )
parseCommand :: Parser Command
parseCommand = subparser $
        command "fetch"     (parseFetch   `withInfo`
            "Fetch transaction data from the server.")
     <> command "convert"   (parseConvert `withInfo`
            "Convert a a file containing transaction data to ledger entries.")

parseOptions :: Parser Options
parseOptions = Options <$>
    (GlobalOptions <$>
        parseConfPath
    <*> parseRulePath)
    <*> parseCommand

parseConfPath :: Parser (Maybe FilePath)
parseConfPath =
    ( optional $ strOption $
      long "config"
  <>  short 'c'
  <>  metavar "FILENAME"
  <>  help "Config file to use" )

parseRulePath :: Parser (Maybe FilePath)
parseRulePath =
    ( optional $ strOption $
      long "rules"
  <>  short 'r'
  <>  metavar "FILENAME"
  <>  help "Rules file to use" )

opts = info (parseOptions <**> helper)
    ( fullDesc
   <> progDesc "Fetch transaction data and convert it to ledger transactions."
   <> Options.Applicative.header "bankfetcher - a bank transaction fetcher"
    )

execArgparse = execParser opts
