{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib

import Data.Tuple
import Control.Monad
import Control.Monad.IO.Class
import Debug.Trace

import Options.Applicative
import Control.Applicative
import Data.Semigroup ((<>))

import qualified Network.Wreq as Request
import qualified Network.Wreq.Session as S

import qualified Data.ByteString.Lazy as B


jsonFile :: FilePath
jsonFile = "pizza.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

-- login
-- https://net.pbz.hr/pbz365/logonForm.htm
--
-- URL for account data via json
-- https://net.pbz.hr/pbz365/accountTurnovers/currentAccTurnovers.json

data Options = Options
    { period :: Period
    }

periodReader :: ReadM (Period)
periodReader = eitherReader $ \arg ->
    case parseMaybe periodParser arg of
      Nothing -> Left $ trace(arg) $ ("Cannot parse date: " ++ arg)
      Just period -> Right period

argparse :: Parser Options
argparse = Options
      <$> option periodReader
          ( long "period"
         <> short 'p'
         <> metavar "PERIOD"
         <> help "Fetch transactions only for the given time-period." )

main :: IO ()
main = parser =<< execParser opts
  where
    opts = info (argparse <**> helper)
      ( fullDesc
     <> progDesc "Fetch transaction data and convert it to ledger transactions."
     <> header "bankfetcher - a bank transaction fetcher" )

parser :: Options -> IO ()
parser (Options p) = putStrLn $ "Hello " ++ show p
