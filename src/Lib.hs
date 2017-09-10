{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib
    ( Period
    , Transaction(..)
    , Transactions
    , Password
    , renderPrettyTransactions
    , transformTransactions
    , getJSON
    ) where

import Rules

import Data.List
import Data.Time
import Data.Maybe
import Data.Aeson

import GHC.Generics

import System.IO

import Text.Printf
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
import Text.Regex.PCRE

import qualified Data.ByteString.Lazy as B

type Password = String
type Period = (Day, Day)

type Transactions = [Transaction]

data Transaction = Transaction
    { id          :: String
    , date        :: Day
    , description :: String
    , payAmount   :: Maybe Float
    , recAmount   :: Maybe Float
    , currency    :: String
    } deriving (Generic, Show)

instance ToJSON Transaction
instance FromJSON Transaction

-- TODO pay and rec don't necessarily have to be in the same currency
-- TODO the accounts should be part of the transaction
-- TODO cleanup
instance Pretty Transaction where
    pPrint (Transaction id date description payAmount recAmount currency) =
        d <+> char '*' <+> desc
            $$ (nest ident $ targetAcc <+> pay <+> cur <+> semi <+> i)
            $$ (nest ident $ sourceAcc <+> rec <+> cur)
            where
                ident     = 4
                i         = text id
                d         = text $ formatTime defaultTimeLocale "%Y/%m/%d" date
                desc      = text description
                pay       = prettyFloat $ fromMaybe 0.00 payAmount
                rec       = prettyFloat $ fromMaybe 0.00 recAmount
                cur       = text currency
                sourceAcc = text $ printf accFormat ("Assets:PBZ"   :: String)
                targetAcc = text $ printf accFormat ("Expenses:???" :: String)
                accFormat = "%-30s"

prettyFloat :: Float -> Doc
prettyFloat f = text $ printf "%10.2f" f

renderPrettyTransactions :: Transactions -> String
renderPrettyTransactions ts = intercalate "\n\n" $ map prettyShow ts

patternMatches :: Transaction -> Pattern -> Bool
patternMatches (Transaction _ _ obj _ _ _) (Pattern Description Is args)      = any (obj ==) args
patternMatches (Transaction _ _ _ _ _ obj) (Pattern Currency Is args)         = any (obj ==) args
patternMatches (Transaction _ _ obj _ _ _) (Pattern Description Matches args) = any (obj =~) args
patternMatches (Transaction _ _ _ _ _ obj) (Pattern Currency Matches args)    = any (obj =~) args

ruleMatches :: Transaction -> Rule -> Bool
ruleMatches t (Rule _ p _) = all (patternMatches t) p

executeAction :: Transaction -> Action -> Transaction
executeAction t (Action Set Description arg) = t { description = arg }
executeAction t (Action Set Currency arg)    = t { currency    = arg }

findMatchingRule :: Rules -> Transaction -> Maybe Rule
findMatchingRule rs t = listToMaybe $ filter (ruleMatches t) rs

transformTransaction :: Rules -> Transaction -> Transaction
transformTransaction rs t = foldl executeAction t a
    where (Rule _ _ a) = fromMaybe (Rule "" [] []) $ findMatchingRule rs t

transformTransactions :: Rules -> Transactions -> Transactions
transformTransactions r t = map (transformTransaction r) t

getJSON :: Maybe FilePath -> IO B.ByteString
getJSON (Just file) = B.readFile file
getJSON Nothing     = B.hGetContents stdin
