{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib

import Data.String

import Data.Time
import Data.Time.Format
import Data.Tuple
import Control.Monad
import Control.Monad.IO.Class
import Debug.Trace

import Options.Applicative
import Control.Applicative
import Data.Semigroup ((<>))

import qualified Network.Wreq as Request
import qualified Network.Wreq.Session as S

import qualified Data.ByteString.Lazy as B

getJSON :: FilePath -> IO B.ByteString
getJSON jsonFile = B.readFile jsonFile

-- login
-- https://net.pbz.hr/pbz365/logonForm.htm
--
-- URL for account data via json
-- https://net.pbz.hr/pbz365/accountTurnovers/currentAccTurnovers.json

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

data Options = Options Command

main :: IO ()
main = run =<< execParser opts
    where
        opts = info (parseOptions <**> helper)
            ( fullDesc
           <> progDesc "Fetch transaction data and convert it to ledger transactions."
           <> header "bankfetcher - a bank transaction fetcher"
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

parseCommand :: Parser Command
parseCommand = subparser $
        command "fetch"     (parseFetch   `withInfo`
            "Fetch transaction data from the server.")
     <> command "convert"   (parseConvert `withInfo`
            "Convert a a file containing transaction data to ledger entries.")

parseOptions :: Parser Options
parseOptions = Options <$> parseCommand

run :: Options -> IO ()
run (Options cmd) = do
    case cmd of
      Fetch period -> putStrLn $ show period
      Convert file ->
          case file of
            Nothing   -> undefined
            Just file -> do
                inputData <- getJSON file
                mapM_ printTransaction $ fromPBZ inputData
