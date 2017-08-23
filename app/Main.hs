{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import PBZ
import Rules
import Config
import Argparse

import Control.Exception

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

main :: IO ()
main = run =<< execArgparse
