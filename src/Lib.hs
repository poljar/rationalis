{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( Period
    , Transaction(..)
    , Transactions
    , renderPrettyTransactions
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
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
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

-- TODO pay and rec don't necessarily have to be in the same currency
-- TODO the accounts should be part of the transaction
-- TODO cleanup
instance Pretty Transaction where
    pPrint (Transaction id date description payAmount recAmount currency) =
        d <+> char '*' <+> desc
            $$ (nest 4 $ targetAcc <+> pay <+> cur <+> semi <+> i)
            $$ (nest 4 $ sourceAcc <+> rec <+> cur)
            where
                i         = text id
                d         = text $ formatTime defaultTimeLocale "%Y/%m/%d" date
                desc      = text description
                pay       = prettyFloat $ fromMaybe 0.00 payAmount
                rec       = prettyFloat $ fromMaybe 0.00 recAmount
                cur       = text currency
                sourceAcc = text $ printf "%-30s" ("Assets:PBZ"   :: String)
                targetAcc = text $ printf "%-30s" ("Expenses:???" :: String)

prettyFloat :: Float -> Doc
prettyFloat f = text $ printf "%10.2f" f

renderPrettyTransaction :: Transaction -> String
renderPrettyTransaction t = prettyShow t

renderPrettyTransactions :: Transactions -> [String]
renderPrettyTransactions ts = map renderPrettyTransaction ts

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
