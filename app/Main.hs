{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import PBZ
import Rules
import Config
import Argparse

import Control.Exception

import Data.List
import Data.Maybe
import Data.ConfigFile (CPError)

import System.Exit
import System.Directory
import System.FilePath

import Text.Megaparsec.Error (parseErrorPretty)

getRules :: FilePath -> IO Rules
getRules f = do
    eitherRule <- parseRulesFile f
    case eitherRule of
      Left err -> die (parseErrorPretty err)
      Right rs -> return rs

-- TODO there has to be a better way to do this.
tryGetRules :: FilePath -> IO Rules
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

writeTransactions :: Maybe FilePath -> Transactions -> IO ()
writeTransactions Nothing ts  = putStrLn $ intercalate "\n\n"  $ renderPrettyTransactions ts
writeTransactions (Just f) ts = writeFile f $ intercalate "\n" $ renderPrettyTransactions ts

runConvert :: Maybe FilePath -> Maybe FilePath -> Rules -> IO ()
runConvert inFile outFile rules = do
    inputData <- getJSON inFile
    let ts = transformTransactions rules (fromPBZ inputData)
    writeTransactions outFile ts

run :: Options -> IO ()
run (Options globOpts cmd) = do
    (confFile, ruleFile) <- fromMaybeGlobalOpts globOpts

    conf    <- tryGetConf  confFile
    rules   <- tryGetRules ruleFile

    case cmd of
      Fetch period -> print =<< fetchPBZ period
      Convert inFile outFile -> runConvert inFile outFile rules

main :: IO ()
main = run =<< execArgparse
