{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( Period
    , Transaction(..)
    , Amount(..)
    , Posting(..)
    , Transactions
    , Password
    , renderPrettyTransactions
    , transformTransactions
    , getJSON
    ) where

import Rules

import Data.Aeson
import Data.List
import Data.Maybe
import Data.Time

import GHC.Generics

import System.IO

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
import Text.Printf
import Text.Regex.PCRE

import qualified Data.ByteString.Lazy as B

type Password = String

type Period = (Day, Day)

type Transactions = [Transaction]

data Transaction = Transaction
    { transactionID :: String
    , transactionDate :: Day
    , description :: String
    , fromPosting :: Posting
    , toPosting :: Posting
    } deriving (Generic, Show)

data Amount =
    Amount Float
           String
    deriving (Generic, Show, Eq)

instance ToJSON Amount

instance FromJSON Amount

instance Pretty Amount where
    pPrint (Amount n c) = prettyFloat n <+> text c

data Posting =
    Posting String
            Amount
    deriving (Generic, Show)

instance Pretty Posting where
    pPrint (Posting account amount) = text prettyAcc <+> pPrint amount
      where
        prettyAcc = printf accFormat account
        accFormat = "%-30s"

instance ToJSON Posting

instance FromJSON Posting

instance ToJSON Transaction

instance FromJSON Transaction

instance Eq Transaction where
    (Transaction id1 _ _ _ _) == (Transaction id2 _ _ _ _) = id1 == id2

instance Ord Transaction where
    compare (Transaction _ d1 _ _ _) (Transaction _ d2 _ _ _) = compare d1 d2

instance Pretty Transaction where
    pPrint (Transaction tID day d p r) =
        date <+>
        char '*' <+>
        desc $$ nest ident (pPrint p <+> semi <+> i) $$ nest ident (pPrint r)
      where
        ident = 4
        i = text "ID:" <+> text tID
        date = text $ formatTime defaultTimeLocale "%Y/%m/%d" day
        desc = text d

prettyFloat :: Float -> Doc
prettyFloat f = text $ printf "%10.2f" f

renderPrettyTransactions :: Transactions -> String
renderPrettyTransactions ts = intercalate "\n\n" $ map prettyShow ts

patternMatches :: Transaction -> Pattern -> Bool
patternMatches (Transaction _ _ obj _ _) (Pattern Description Is args) =
    obj `elem` args
-- patternMatches (Transaction _ _ _ _ _ obj) (Pattern Currency Is args) =
--     obj `elem` args
patternMatches (Transaction _ _ obj _ _) (Pattern Description Matches args) =
    any (obj =~) args

-- patternMatches (Transaction _ _ _ _ _ obj) (Pattern Currency Matches args) =
--     any (obj =~) args
ruleMatches :: Transaction -> Rule -> Bool
ruleMatches t (Rule _ p _) = all (patternMatches t) p

executeAction :: Transaction -> Action -> Transaction
executeAction t (Action Set Description arg) = t {description = arg}

-- executeAction t (Action Set Currency arg) = t {currency = arg}
findMatchingRule :: Rules -> Transaction -> Maybe Rule
findMatchingRule rs t = listToMaybe $ filter (ruleMatches t) rs

transformTransaction :: Rules -> Transaction -> Transaction
transformTransaction rs t = foldl executeAction t a
  where
    (Rule _ _ a) = fromMaybe (Rule "" [] []) $ findMatchingRule rs t

transformTransactions :: Rules -> Transactions -> Transactions
transformTransactions r = map (transformTransaction r)

getJSON :: Maybe FilePath -> IO B.ByteString
getJSON (Just file) = B.readFile file
getJSON Nothing = B.hGetContents stdin
