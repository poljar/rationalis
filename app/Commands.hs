{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Commands where

import Lib
import Rules
import Config
import Argparse

import Data.Aeson
import Data.Maybe
import Data.Time.Calendar

import System.Exit
import System.Process.Typed

import qualified Data.ByteString.Lazy.Char8 as L

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
