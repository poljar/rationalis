{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( Period
    , Transaction(..)
    , Amount(..)
    , Posting(..)
    , Transactions
    , Password
    , transformTransactions
    , getJSON
    , tryGetFile
    ) where

import Rules

import Control.Exception

import Data.Aeson
import Data.List
import Data.Maybe
import Data.Time

import GHC.Generics

import System.IO

import Text.PrettyPrint.ANSI.Leijen
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
    , payerPosting :: Posting
    , payeePosting :: Posting
    } deriving (Generic, Show)

data Amount =
    Amount Float
           String
    deriving (Generic, Show, Eq)

instance ToJSON Amount

instance FromJSON Amount

instance Pretty Amount where
    pretty (Lib.Amount n c) = numberColor $ prettyFloat n <+> text c
      where
        numberColor = magenta

data Posting =
    Posting String
            Amount
    deriving (Generic, Show)

instance Pretty Posting where
    pretty (Posting account amount) = prettyAcc <+> pretty amount
      where
        prettyAcc = accountColor $ fill 30 $ text account
        accountColor = cyan

instance ToJSON Posting

instance FromJSON Posting

instance ToJSON Transaction

instance FromJSON Transaction

instance Eq Transaction where
    (Transaction id1 _ _ _ _) == (Transaction id2 _ _ _ _) = id1 == id2

instance Ord Transaction where
    compare (Transaction _ d1 _ _ _) (Transaction _ d2 _ _ _) = compare d1 d2

instance Pretty Transaction where
    pretty (Transaction tID day d p r) =
        hang indentation (header <$$> pretty p <+> semi <+> i <$$> pretty r)
      where
        indentation = 4
        header = date <+> char '*' <+> desc
        i = text "ID:" <+> idColor (text tID)
        date = dateColor $ text $ formatTime defaultTimeLocale "%Y/%m/%d" day
        desc = descColor $ text d
        dateColor = blue
        descColor = yellow
        idColor = magenta
    prettyList = v2sep . map pretty

v2sep :: [Doc] -> Doc
v2sep [] = Text.PrettyPrint.ANSI.Leijen.empty
v2sep [x] = x
v2sep (x:xs) = x <> line <> line <> v2sep xs

prettyFloat :: Float -> Doc
prettyFloat f = text $ printf "%10.2f" f

patternMatches :: Transaction -> Pattern -> Bool
patternMatches
    (Transaction _ _ obj _ _)
    (Pattern (Left Description) Is args) = obj `elem` args

patternMatches
    (Transaction _ _ obj _ _)
    (Pattern (Left Description) Matches args) = any (obj =~) args

ruleMatches :: Transaction -> Rule -> Bool
ruleMatches t (Rule _ p _) = all (patternMatches t) p

executeAction :: Transaction -> Action -> Transaction
executeAction t (Action Set (Left Description) arg) = t {description = arg}

findMatchingRule :: Rules -> Transaction -> Maybe Rule
findMatchingRule rs t = listToMaybe $ filter (ruleMatches t) rs

transformTransaction :: Rules -> Transaction -> Transaction
transformTransaction rs t = foldl executeAction t a
  where
    (Rule _ _ a) = fromMaybe (Rule "" [] []) $ findMatchingRule rs t

transformTransactions :: Rules -> Transactions -> Transactions
transformTransactions r ts = map (transformTransaction r) (sort ts)

getJSON :: Maybe FilePath -> IO B.ByteString
getJSON (Just file) = B.readFile file
getJSON Nothing = B.hGetContents stdin

tryGetFile ::
       forall t. (Monoid t)
    => FilePath
    -> (FilePath -> IO t)
    -> IO t
tryGetFile f fileReader = do
    ret <- try $ fileReader f :: IO (Either IOException t)
    case ret of
        Left _ -> mempty
        Right value -> return value
