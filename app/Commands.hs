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
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import System.Exit
import System.IO
import System.FilePath
import System.Process.Typed

import qualified Data.ByteString.Lazy.Char8 as L

writeTransactions :: Maybe FilePath -> Transactions -> IO ()
writeTransactions Nothing ts = do
    PP.putDoc $ PP.pretty ts
    putStrLn ""

writeTransactions (Just f) ts = do
    handle <- openFile f WriteMode
    PP.hPutDoc handle $ PP.pretty ts
    hClose handle

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
       FilePath
    -> Maybe String
    -> Maybe Password
    -> Maybe Period
    -> ProcessConfig () () ()
createProcConf f user pw p = setEnv env $ proc f []
  where
    env =
        [ ("RATIONALIS_USER", fromMaybe "" user)
        , ("RATIONALIS_PASSWORD", fromMaybe "" pw)
        , ("RATIONALIS_FROM_DATE", from)
        , ("RATIONALIS_TO_DATE", to)
        , ("RATIONALIS_LAST_ID", "")
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
    => FilePath
    -> Maybe String
    -> Maybe Password
    -> Maybe Period
    -> m (L.ByteString, L.ByteString)
runFetcher f user pass period =
    readProcess_ $ createProcConf f user pass period

runFetch :: FetchOptions -> IO ()
runFetch (FetchOptions f user pass period file) = do
    (out, err) <- runFetcher f user pass period
    L.putStr err
    writeFetchedFile file out

runPull :: String -> Rules -> Config -> IO ()
runPull a r c = do
    acc <- getAccountOrDie a c
    (out, err) <- runFetcher (fetcher acc) (Just $ userName acc) Nothing Nothing
    let trans = decode out :: Maybe Transactions
    case trans of
        Just ts -> writeTransactions Nothing $ transformTransactions r (sort ts)
        Nothing -> die "Error: Unable to parse input file."
