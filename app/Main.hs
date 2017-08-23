{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import PBZ
import Rules
import Config

import System.Exit
import System.Directory
import System.FilePath
import Data.String
import Data.ConfigFile (CPError)
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
import Control.Exception
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

data GlobalOptions = GlobalOptions
    { confPath :: Maybe FilePath
    , rulePath :: Maybe FilePath
    }

data Options = Options
    { globalOpts :: GlobalOptions
    , cmd        :: Command
    }

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

-- TODO there has to be a better way to do this.
tryGetRules :: FilePath -> IO [Rule]
tryGetRules file = do
    ret <- try $ getRules file :: IO (Either IOException Rules)
    case ret of
      Left err -> return []
      Right as -> return as

tryGetConf :: FilePath -> IO (Maybe (Either CPError Config))
tryGetConf file = do
    ret <- try $ readConf file :: IO (Either IOException (Either CPError Config))
    case ret of
      Left err -> return Nothing
      Right cp -> return (Just cp)

fromMaybeGlobalOpts :: GlobalOptions -> IO (FilePath, FilePath)
fromMaybeGlobalOpts (GlobalOptions mConfFile mRuleFile) = do
    confDir <- getXdgDirectory XdgConfig "rationalis"

    confFile <- case mConfFile of
        Nothing -> return (joinPath [confDir, "rationalis.conf"])
        Just f  -> return f

    ruleFile <- case mRuleFile of
        Nothing -> return (joinPath [confDir, "rationalis.rules"])
        Just f  -> return f
    return (confFile, ruleFile)

runConvert :: Maybe FilePath -> Rules -> IO ()
runConvert file rules = do
    case file of
        Nothing -> undefined

        Just file -> do
            inputData <- getJSON file
            let outputData = transformTransactions rules (fromPBZ inputData)
            mapM_ printTransaction outputData

run :: Options -> IO ()
run (Options globOpts cmd) = do
    (confFile, ruleFile) <- fromMaybeGlobalOpts globOpts

    conf    <- tryGetConf  confFile
    rules   <- tryGetRules ruleFile

    case cmd of
      Fetch period -> print =<< fetchPBZ period
      Convert file -> runConvert file rules
