{-# LANGUAGE DeriveGeneric #-}

module Commands where

import Argparse
import Config
import Lib
import Rules hiding (Account)
import Transaction
import Transformations

import Control.Arrow
import Control.Monad.IO.Class (MonadIO)

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Maybe
import Data.Time
import Text.PrettyPrint.ANSI.Leijen (pretty)

import GHC.Generics (Generic)

import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process.Typed

import qualified Data.ByteString.Lazy.Char8 as L

writeTransactions :: Maybe FilePath -> Transactions -> IO ()
writeTransactions Nothing ts = do
    hPutDoc' stdout $ pretty ts
    putStrLn ""
writeTransactions (Just f) ts = do
    handle <- openFile f WriteMode
    hPutDoc' handle $ pretty ts
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
    -> Maybe String
    -> ProcessConfig () () ()
createProcConf f user pw p lastId = setEnv env $ proc f []
  where
    env =
        [ ("RATIONALIS_USER", fromMaybe "" user)
        , ("RATIONALIS_PASSWORD", fromMaybe "" pw)
        , ("RATIONALIS_FROM_DATE", from)
        , ("RATIONALIS_TO_DATE", to)
        , ("RATIONALIS_LAST_ID", fromMaybe "" lastId)
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
    -> Maybe String
    -> m (L.ByteString, L.ByteString)
runFetcher f user pass period lastId =
    readProcess_ $ createProcConf f user pass period lastId

runFetch :: FetchOptions -> IO ()
runFetch (FetchOptions f user pass period file) = do
    (out, err) <- runFetcher f user pass period Nothing
    L.putStr err
    case (decode out :: Maybe Transactions) of
        Just ts -> writeFetchedFile file $ encodePretty ts
        Nothing -> die "Error: Unable to parse fetched transactions."

type History = [HistEntry]

data HistEntry =
    HistEntry String
              String
              Day
    deriving (Generic, Show)

instance ToJSON HistEntry

instance FromJSON HistEntry

instance Eq HistEntry where
    (HistEntry acc1 _ _) == (HistEntry acc2 _ _) = acc1 == acc2

updateHistory :: String -> Transaction -> History -> History
updateHistory acc t hs = newentry : filter (newentry /=) hs
  where
    newentry = HistEntry acc (transactionID t) (transactionDate t)

getHistFile :: IO FilePath
getHistFile = do
    dataDir <- getXdgDirectory XdgData "rationalis"
    createDirectoryIfMissing False dataDir
    return $ dataDir </> "history"

readHistory :: FilePath -> IO History
readHistory histFile = do
    maybeHistory <-
        decode <$> tryGetFile histFile L.readFile :: IO (Maybe History)
    return $ fromMaybe [] maybeHistory

writeHistory :: String -> Transaction -> IO ()
writeHistory acc t = do
    histFile <- getHistFile
    history <- readHistory histFile
    let tmpFile = histFile <.> "new"
    L.writeFile tmpFile (encodePretty $ updateHistory acc t history)
    renameFile tmpFile histFile

mutateName :: Integer -> FilePath -> [FilePath] -> FilePath
mutateName num name entries =
    if newName `elem` entries
        then mutateName (succ num) name entries
        else newName
  where
    newName = baseName ++ "-" ++ show num <.> "ldg"
    baseName = dropExtension name

generateFileName :: String -> Day -> [FilePath] -> FilePath
generateFileName accName day entries =
    if name `elem` entries
        then mutateName 1 name entries
        else name
  where
    name = accName ++ "-" ++ dateString <.> "ldg"
    dateString = showGregorian day

getOutFile :: Account -> IO FilePath
getOutFile acc = do
    createDirectoryIfMissing False dir
    entries <- listDirectory dir
    today <- utctDay <$> getCurrentTime
    return $ dir </> generateFileName accName today entries
  where
    dir = outDir acc
    accName = accountName acc

runPull :: String -> Maybe Password -> Rules -> Config -> IO ()
runPull a pw r c = do
    acc <- getAccountOrDie a c
    -- TODO pass the history to the fetcher
    (out, err) <-
        runFetcher (fetcher acc) (Just $ userName acc) pw Nothing Nothing
    L.hPutStr stderr err
    fileName <- getOutFile acc
    let trans = decode out :: Maybe Transactions
    case trans of
        Just ts -> do
            writeTransactions (Just fileName) (transformTransactions r ts)
            writeHistory (accountName acc) (last ts)
        Nothing -> die "Error: Unable to parse input file."
