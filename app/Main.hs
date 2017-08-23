{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import PBZ
import Rules
import Config
import Argparse

import Control.Exception

import Data.Time
import Data.Maybe
import Data.ConfigFile (CPError)

import System.Exit
import System.Directory
import System.FilePath

import Text.Regex.PCRE
import Text.Megaparsec.Error

import qualified Data.ByteString.Lazy as B

getJSON :: FilePath -> IO B.ByteString
getJSON jsonFile = B.readFile jsonFile

main :: IO ()
main = run =<< execArgparse

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
