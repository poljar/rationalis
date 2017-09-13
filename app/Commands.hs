{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Commands where

import Argparse
import Config
import Lib
import Rules

import Control.Arrow
import Control.Monad.IO.Class (MonadIO)

import Data.Aeson
import Data.List
import Data.Maybe
import Data.Time.Calendar

import System.Exit
import System.FilePath
import System.Process.Typed

import qualified Data.ByteString.Lazy.Char8 as L

writeTransactions :: Maybe FilePath -> Transactions -> IO ()
writeTransactions Nothing ts = putStrLn $ renderPrettyTransactions ts
writeTransactions (Just f) ts = writeFile f $ renderPrettyTransactions ts

runConvert :: Maybe FilePath -> Maybe FilePath -> Rules -> IO ()
runConvert inFile outFile rules = do
    inputData <- decode <$> getJSON inFile
    case inputData of
        Just ts -> writeTransactions outFile $ transformTransactions rules ts
        Nothing -> die "Error: Unable to parse input file."

writeFetchedFile :: Maybe FilePath -> L.ByteString -> IO ()
writeFetchedFile Nothing out = L.putStr out
writeFetchedFile (Just f) out = L.writeFile f out

createProcConf ::
       Maybe Period
    -> String
    -> Maybe Password
    -> FilePath
    -> ProcessConfig () () ()
createProcConf p user pw f = setEnv env $ proc f []
  where
    env =
        [ ("RATIONALIS_USER", user)
        , ("RATIONALIS_PASSWORD", fromMaybe "" pw)
        , ("RATIONALIS_FROM_DATE", from)
        , ("RATIONALIS_TO_DATE", to)
        ]
    (from, to) =
        case p of
            Nothing -> ("", "")
            Just period -> (showGregorian *** showGregorian) period

filterAccount :: String -> Account -> Bool
filterAccount target (Account name _ _ _) = target == name

findAccount :: String -> Config -> Maybe Account
findAccount targetAcc (Config accs) = listToMaybe accounts
  where
    accounts = filter (filterAccount targetAcc) accs

getAccountOrDie :: String -> Config -> IO Account
getAccountOrDie targetAcc conf =
    case account of
        Nothing -> die $ "Account '" ++ targetAcc ++ "' not found."
        Just a -> return a
  where
    account = findAccount targetAcc conf

-- TODO don't read stderr, just leave it connected to the parent stderr
-- TODO handle failed processes more gracefully
runFetcher ::
       MonadIO m
    => Account
    -> Maybe Period
    -> Maybe Password
    -> m (L.ByteString, L.ByteString)
runFetcher acc period pass =
    readProcess_ $ createProcConf period (userName acc) pass (fetcher acc)

runFetch :: FetchOptions -> Config -> IO ()
runFetch (FetchOptions targetAcc period file pass) conf = do
    acc <- getAccountOrDie targetAcc conf
    (out, err) <- runFetcher acc period pass
    L.putStr err
    writeFetchedFile file out

runPull :: String -> Rules -> Config -> IO ()
runPull a r c = do
    acc <- getAccountOrDie a c
    (out, err) <- runFetcher acc Nothing Nothing
    let trans = decode out :: Maybe Transactions
    case trans of
        Just ts -> writeTransactions Nothing $ transformTransactions r (sort ts)
        Nothing -> die "Error: Unable to parse input file."
