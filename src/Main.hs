{-# LANGUAGE OverloadedStrings #-}

module Main where

import Argparse
import Commands
import Config
import Lib
import Rules

import System.Directory
import System.Exit
import System.FilePath

import Text.Megaparsec.Error (parseErrorPretty)

getFile :: FilePath -> (FilePath -> IO (Either t b)) -> (t -> String) -> IO b
getFile f reader errorPrinter = do
    ret <- reader f
    case ret of
        Left err -> die $ errorPrinter err
        Right val -> return val

getRules :: FilePath -> IO Rules
getRules f = getFile f parseRulesFile parseErrorPretty

getConf :: FilePath -> IO Config
getConf f = getFile f readConf confErrorPretty

tryGetRules :: FilePath -> IO Rules
tryGetRules file = tryGetFile file getRules

tryGetConf :: FilePath -> IO Config
tryGetConf file = tryGetFile file getConf

checkFile :: FilePath -> IO FilePath
checkFile f = do
    exists <- doesFileExist f
    if exists
        then return f
        else die $ "No such file: " ++ show f

fromMaybeGlobalOpts :: GlobalOptions -> IO (FilePath, FilePath)
fromMaybeGlobalOpts (GlobalOptions mConfFile mRuleFile) = do
    confDir <- getXdgDirectory XdgConfig "rationalis"
    confFile <-
        case mConfFile of
            Nothing -> return (joinPath [confDir, "config"])
            Just f -> checkFile f
    ruleFile <-
        case mRuleFile of
            Nothing -> return (joinPath [confDir, "rules"])
            Just f -> checkFile f
    return (confFile, ruleFile)

run :: Options -> IO ()
run (Options globOpts cmd) = do
    (confFile, ruleFile) <- fromMaybeGlobalOpts globOpts
    conf <- tryGetConf confFile
    rules <- tryGetRules ruleFile
    case cmd of
        Fetch fetchOpts -> runFetch fetchOpts
        Convert inFile outFile -> runConvert inFile outFile rules
        Pull acc pw -> runPull acc pw rules conf

main :: IO ()
main = run =<< execArgparse
