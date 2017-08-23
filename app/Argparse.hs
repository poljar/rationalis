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
import Data.Maybe
import Data.Time.Format
import Options.Applicative
import Control.Applicative
import Data.Semigroup ((<>))

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

data Command
    = Fetch Period
    | Convert (Maybe FilePath)

data GlobalOptions = GlobalOptions
    { confPath :: Maybe FilePath
    , rulePath :: Maybe FilePath
    }

data Options = Options
    { globalOpts :: GlobalOptions
    , cmd        :: Command
    }

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
