{-# LANGUAGE DeriveGeneric #-}

module Transaction
    ( Transaction(..)
    , Amount(..)
    , Posting(..)
    , Transactions
    ) where

import Data.Aeson
import Data.Time

import GHC.Generics

import Text.PrettyPrint.ANSI.Leijen
import Text.Printf

type Transactions = [Transaction]

data Transaction = Transaction
    { transactionID :: String
    , transactionDate :: Day
    , description :: String
    , payerPosting :: Posting
    , payeePosting :: Posting
    , transactionComment :: Maybe String
    } deriving (Generic, Show)

data Amount =
    Amount Float
           String
    deriving (Generic, Show, Eq)

instance ToJSON Amount

instance FromJSON Amount

instance Pretty Amount where
    pretty (Amount n c) = numberColor $ prettyFloat n <+> text c
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
    (Transaction id1 _ _ _ _ _) == (Transaction id2 _ _ _ _ _) = id1 == id2

instance Ord Transaction where
    compare (Transaction _ d1 _ _ _ _) (Transaction _ d2 _ _ _ _) =
        compare d1 d2

instance Pretty Transaction where
    pretty (Transaction tID day d p r c) =
        hang
            indentation
            (header <$$> pretty p <+> semi <+> i <$$> pretty r <+> comment)
      where
        indentation = 4
        header = date <+> char '*' <+> desc
        i = text "ID:" <+> idColor (text tID)
        date = dateColor $ text $ formatTime defaultTimeLocale "%Y/%m/%d" day
        desc = descColor $ text d
        comment =
            case c of
                Nothing -> text ""
                Just v -> text ";" <+> text v
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
