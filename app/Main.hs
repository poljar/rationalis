{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Lib
import PBZ
import Rules
import Config
import Argparse

import Control.Exception

import Data.Aeson
import Data.ConfigFile (CPError)

import System.Exit
import System.Directory
import System.FilePath
import System.Process.Typed

import Text.Megaparsec.Error (parseErrorPretty)

getRules :: FilePath -> IO Rules
getRules f = do
    eitherRule <- parseRulesFile f
    case eitherRule of
      Left err -> die (parseErrorPretty err)
      Right rs -> return rs

getConf :: FilePath -> IO Config
getConf f = do
    eitherConf <- readConf f
    case eitherConf of
      Left err   -> die $ "Error parsing conf: " ++ (show err)
      Right conf -> return conf

tryGetFile :: forall t. (Monoid t) => FilePath -> (FilePath -> IO t) -> IO t
tryGetFile f fileReader = do
    ret <- try $ fileReader f :: IO (Either IOException t)
    case ret of
      Left _ -> mempty
      Right value -> return value

tryGetRules :: FilePath -> IO Rules
tryGetRules file = tryGetFile file getRules

tryGetConf :: FilePath -> IO Config
tryGetConf file = tryGetFile file getConf

checkFile :: FilePath -> IO FilePath
checkFile f = do
    exists <- doesFileExist f
    case exists of
      True -> return f
      False -> die $ "No such file: " ++ (show f)

fromMaybeGlobalOpts :: GlobalOptions -> IO (FilePath, FilePath)
fromMaybeGlobalOpts (GlobalOptions mConfFile mRuleFile) = do
    confDir <- getXdgDirectory XdgConfig "rationalis"

    confFile <- case mConfFile of
        Nothing -> return (joinPath [confDir, "rationalis.conf"])
        Just f  -> checkFile f

    ruleFile <- case mRuleFile of
        Nothing -> return (joinPath [confDir, "rationalis.rules"])
        Just f  -> checkFile f
    return (confFile, ruleFile)

writeTransactions :: Maybe FilePath -> Transactions -> IO ()
writeTransactions Nothing ts  = putStrLn $ renderPrettyTransactions ts
writeTransactions (Just f) ts = writeFile f $ renderPrettyTransactions ts

runConvert :: Maybe FilePath -> Maybe FilePath -> Rules -> IO ()
runConvert inFile outFile rules = do
    inputData <- decode <$> getJSON inFile

    case inputData of
        Just ts -> writeTransactions outFile $ transformTransactions rules ts
        Nothing -> die "Error: Unable to parse input file."

runFetch :: FetchOptions -> Config -> IO ()
runFetch (FetchOptions period file pass) conf = runProcess "true" >>= print

run :: Options -> IO ()
run (Options globOpts cmd) = do
    (confFile, ruleFile) <- fromMaybeGlobalOpts globOpts

    conf    <- tryGetConf  confFile
    rules   <- tryGetRules ruleFile

    case cmd of
      Fetch fetchOpts         -> runFetch fetchOpts conf
      Convert inFile outFile  -> runConvert inFile outFile rules

main :: IO ()
main = run =<< execArgparse
