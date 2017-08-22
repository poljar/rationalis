{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Rules
import PBZ

import System.Exit
import Data.String
import Text.Regex.PCRE

import Data.Time
import Data.Maybe
import Data.Time.Format
import Data.Tuple
import Control.Monad
import Control.Monad.IO.Class
import Debug.Trace

import Text.Megaparsec.Error
import Options.Applicative
import Control.Applicative
import Data.Semigroup ((<>))

import qualified Data.ByteString.Lazy as B

getJSON :: FilePath -> IO B.ByteString
getJSON jsonFile = B.readFile jsonFile

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
           <> Options.Applicative.header "bankfetcher - a bank transaction fetcher"
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

day :: Day
day = fromGregorian 2017 20 1
trans = Transaction "PBZ-1495678335" day "PBZ ATM KONZUM BELI MANASTIR" (Just 100.00) Nothing "HRK"
paty = Pattern Description Matches ["PBZ ATM*"]
acy  = Action Set Description "Test"
ruly = [Rule "Test" [paty] [acy]]

patternMatches :: Transaction -> Pattern -> Bool
patternMatches (Transaction _ _ obj _ _ _) (Pattern Description Is args)      = any (obj ==) args
patternMatches (Transaction _ _ _ _ _ obj) (Pattern Currency Is args)         = any (obj ==) args
patternMatches (Transaction _ _ obj _ _ _) (Pattern Description Matches args) = any (obj =~) args
patternMatches (Transaction _ _ _ _ _ obj) (Pattern Currency Matches args)    = any (obj =~) args

ruleMatches :: Transaction -> Rule -> Bool
ruleMatches t (Rule h p a) = all (patternMatches t) p

executeAction :: Transaction -> Action -> Transaction
executeAction t (Action Set Description arg) = t { description = arg }
executeAction t (Action Set Currency arg)    = t { currency    = arg }

findMatchingRule :: Rules -> Transaction -> Maybe Rule
findMatchingRule rs t = listToMaybe $ filter (ruleMatches t) rs

transformTransaction :: Rules -> Transaction -> Transaction
transformTransaction rs t = foldl executeAction t a
    where (Rule h p a) = fromMaybe (Rule "" [] []) $ findMatchingRule rs t

transformTransactions :: Rules -> Transactions -> Transactions
transformTransactions r t = map (transformTransaction r) t

getRules :: FilePath -> IO Rules
getRules f = do
    eitherRule <- parseRulesFile f
    case eitherRule of
      Left err -> die (parseErrorPretty err)
      Right rs -> return rs

run :: Options -> IO ()
run (Options cmd) = do
    rs <- getRules "doc/examples/rationalis.rules"

    case cmd of
      Fetch period -> print =<< fetchPBZ period
      Convert file ->
          case file of
            Nothing   -> undefined
            Just file -> do
                inputData <- getJSON file
                let outputData = transformTransactions rs (fromPBZ inputData)
                mapM_ printTransaction outputData
