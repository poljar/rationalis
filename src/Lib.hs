{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( Period
    , Transaction(..)
    , Transactions
    , printTransaction
    , transformTransactions
    , getJSON
    ) where

import Rules

import Data.Time
import Data.Maybe

import Control.Lens
import Control.Applicative

import System.IO

import Text.Printf
import Text.Regex.PCRE

import qualified Data.ByteString.Lazy as B

type Period = (Day, Day)

type Transactions = [Transaction]

data Transaction = Transaction
    { id          :: String
    , date        :: Day
    , description :: String
    , payAmount   :: Maybe Float
    , recAmount   :: Maybe Float
    , currency    :: String
    } deriving (Show)

patternMatches :: Transaction -> Pattern -> Bool
patternMatches (Transaction _ _ obj _ _ _) (Pattern Description Is args)      = any (obj ==) args
patternMatches (Transaction _ _ _ _ _ obj) (Pattern Currency Is args)         = any (obj ==) args
patternMatches (Transaction _ _ obj _ _ _) (Pattern Description Matches args) = any (obj =~) args
patternMatches (Transaction _ _ _ _ _ obj) (Pattern Currency Matches args)    = any (obj =~) args

ruleMatches :: Transaction -> Rule -> Bool
ruleMatches t (Rule h p a) = all (patternMatches t) p

executeAction :: Transaction -> Action -> Transaction
executeAction t (Action Set Description arg) = t { description = arg }
executeAction t (Action Set Currency arg)    = t { currency    = arg }

findMatchingRule :: Rules -> Transaction -> Maybe Rule
findMatchingRule rs t = listToMaybe $ filter (ruleMatches t) rs

transformTransaction :: Rules -> Transaction -> Transaction
transformTransaction rs t = foldl executeAction t a
    where (Rule h p a) = fromMaybe (Rule "" [] []) $ findMatchingRule rs t

transformTransactions :: Rules -> Transactions -> Transactions
transformTransactions r t = map (transformTransaction r) t

getJSON :: Maybe FilePath -> IO B.ByteString
getJSON (Just file) = B.readFile file
getJSON Nothing     = B.hGetContents stdin

-- TODO indentation should be based on the lengths of the payer/payee accounts
-- some pretty printing lib pretty please?
-- TODO replace ??? using regex based rules
-- TODO pay and rec don't necessarily have to be in the same currency
-- TODO cleanup
printTransaction :: Transaction -> IO ()
printTransaction (Transaction id day description pay rec currency) = do
    putStrLn descriptionLine
    putStrLn payeeLine
    putStrLn payerLine
    putStrLn ""
        where descriptionLine = d ++ " * " ++ description
              d = formatTime defaultTimeLocale "%Y/%m/%d" day
              payeeLine = indent ++ "Expenses:???" ++ indent ++ amountStr
                          ++ " " ++ currency ++ " ; id: " ++ id
              payerLine = indent ++ "Assets:PBZ" ++ indent ++ " -" ++ amountStr
                          ++ " " ++ currency
              indent = "    "
              amountStr = printf "%2f" amount
              amount = case pay of
                    Just pay -> pay
                    Nothing  -> case rec of
                        Just rec -> rec
                        Nothing  -> 0.0
