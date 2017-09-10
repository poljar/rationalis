{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Lib
import Rules
import Config
import Argparse

import Control.Exception

import Data.Aeson
import Data.Maybe
import Data.Time.Calendar

import System.Exit
import System.Directory
import System.FilePath
import System.Process.Typed

import qualified Data.ByteString.Lazy.Char8 as L
import Text.Megaparsec.Error (parseErrorPretty)

getFile :: FilePath -> (FilePath -> IO (Either t b)) -> (t -> String) -> IO b
getFile f reader errorPrinter = do
    ret <- reader f
    case ret of
      Left err -> die $ errorPrinter err
      Right val -> return val

getRules :: FilePath -> IO Rules
getRules f = getFile f parseRulesFile parseErrorPretty

-- TODO better error messages
confErrorPretty err = "Error parsing conf: " ++ (show err)

getConf :: FilePath -> IO Config
getConf f = getFile f readConf confErrorPretty

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

writeFetchedFile :: Maybe FilePath -> L.ByteString -> IO ()
writeFetchedFile Nothing out  = L.putStr out
writeFetchedFile (Just f) out = L.writeFile f out

createProcConf :: Maybe Period -> String -> Maybe Password -> FilePath -> ProcessConfig () () ()
createProcConf p user pw fetcher = setEnv env $ proc fetcher []
    where
        env = [ ("RATIONALIS_USER", user)
              , ("RATIONALIS_PASSWORD", fromMaybe "" pw)
              , ("RATIONALIS_FROM_DATE", from)
              , ("RATIONALIS_TO_DATE", to)
              ]
        (from, to) = case p of
            Nothing -> ("", "")
            Just period -> ( showGregorian $ fst period
                           , showGregorian $ snd period
                           )
filterAccount :: String -> Account -> Bool
filterAccount target (Account name _ _ _) = target == name

findAccount :: String -> Config -> Maybe Account
findAccount targetAcc (Config accs) = listToMaybe $ accounts
    where
        accounts = filter (filterAccount targetAcc) accs

-- TODO don't read stderr, just leave it connected to the parent stderr
-- TODO handle failed processes more gracefully
runFetch :: FetchOptions -> Config -> IO ()
runFetch (FetchOptions targetAcc period file pass) conf = do
    (f, user) <- case account of
           Nothing -> die $ "Account '" ++ targetAcc ++ "' found."
           Just  a -> return $ (fetcher a, userName a)

    (out, err) <- readProcess_ $ createProcConf period user pass f

    L.putStr err
    writeFetchedFile file out
        where
            account = findAccount targetAcc conf

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
