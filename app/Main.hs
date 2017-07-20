{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib

import Data.String
import Control.Lens
import Data.Aeson.Lens

import Data.Maybe
import Data.Time
import Data.Time.Format
import Data.Scientific
import Data.Tuple
import Control.Monad
import Control.Monad.IO.Class
import Debug.Trace

import Options.Applicative
import Control.Applicative
import Data.Semigroup ((<>))

import qualified Data.Text as T
import qualified Network.Wreq as Request
import qualified Network.Wreq.Session as S

import qualified Data.ByteString.Lazy as B


getJSON :: FilePath -> IO B.ByteString
getJSON jsonFile = B.readFile jsonFile

-- login
-- https://net.pbz.hr/pbz365/logonForm.htm
--
-- URL for account data via json
-- https://net.pbz.hr/pbz365/accountTurnovers/currentAccTurnovers.json

data Options = Options
    { period :: Period
    , json   :: FilePath
    }

periodReader :: ReadM (Period)
periodReader = eitherReader $ \arg ->
    case parseMaybe periodParser arg of
      Nothing -> Left $ trace(arg) $ ("Cannot parse date: " ++ arg)
      Just period -> Right period

-- TODO period should be mandatory only for network fetching
argparse :: Parser Options
argparse = Options
      <$> option periodReader
          ( long "period"
         <> short 'p'
         <> metavar "PERIOD"
         <> help "Fetch transactions only for the given time-period." )
      <*>
          strOption
          ( long "json-file"
         <> short 'j'
         <> metavar "JSON"
         <> help "JSON file to use instead of fetching it automatically." )

data Transaction = Transaction
    { date        :: Day
    , description :: String
    , payAmount   :: Maybe Float
    , recAmount   :: Maybe Float
    , currency    :: String
    } deriving (Show)

-- TODO amount shold come from either pay or rec and payee and payer should be
-- swapped depending on pay or rec
-- TODO indentation should be based on the lengths of the payer/payee accounts
-- TODO replace ??? using regex based rules
printTransaction :: Transaction -> IO ()
printTransaction (Transaction day description pay rec currency) = do
    putStrLn descriptionLine
    putStrLn payeeLine
    putStrLn payerLine
        where descriptionLine = d ++ " * " ++ description
              d = formatTime defaultTimeLocale "%Y/%m/%d" day
              payeeLine = indent ++ "Expenses:???" ++ indent ++ (show $ fromJust amount)
              payerLine = indent ++ "Assets:PBZ" ++ indent ++ " -" ++ (show $ fromJust amount)
              indent = "    "
              amount = pay

parseDate :: T.Text -> Day
parseDate s = parseTimeOrError True defaultTimeLocale "%d.%m.%Y. %T" $ T.unpack s

-- TODO ^?! aborts if it can't get the value
filterTransactions :: B.ByteString -> [Transaction]
filterTransactions jsonData = jsonData ^.. members . key "result" . members .
    key "bankAccountTransactionList" . _Array .
    traverse . to (\t -> Transaction
        ( t ^?! key "currencyDate" . _String & parseDate)
        ( t ^?! key "description" . _String & T.unpack)
        ( t ^?  key "payAmount" . key "amount" . _Number & fmap toRealFloat)
        ( t ^?  key "receiveAmount" . key "amount" . _Number & fmap toRealFloat)
        ( t ^?! key "amountAfterTransaction" . key "currency" .
                key "currencyCode" . _String & T.unpack)
        )

main :: IO ()
main = parser =<< execParser opts
  where
    opts = info (argparse <**> helper)
      ( fullDesc
     <> progDesc "Fetch transaction data and convert it to ledger transactions."
     <> header "bankfetcher - a bank transaction fetcher" )

parser :: Options -> IO ()
parser (Options p j) = do
    putStrLn $ "Hello " ++ show p
    jsonData <- getJSON j
    mapM_ printTransaction $ filterTransactions jsonData
