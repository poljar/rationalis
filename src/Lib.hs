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
    , hPutDoc'
    ) where

import Rules

import Control.Exception

import Data.Aeson
import Data.List
import Data.Maybe
import Data.Time

import GHC.Generics

import System.IO
import System.Console.ANSI (hSetSGR, hSupportsANSI)

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
    , transactionComment :: Maybe String
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
    (Transaction id1 _ _ _ _ _) == (Transaction id2 _ _ _ _ _) = id1 == id2

instance Ord Transaction where
    compare (Transaction _ d1 _ _ _ _) (Transaction _ d2 _ _ _ _) = compare d1 d2

instance Pretty Transaction where
    pretty (Transaction tID day d p r c) =
        hang indentation (header
                    <$$> pretty p <+> semi <+> i
                    <$$> pretty r <+> comment)
      where
        indentation = 4
        header = date <+> char '*' <+> desc
        i = text "ID:" <+> idColor (text tID)
        date = dateColor $ text $ formatTime defaultTimeLocale "%Y/%m/%d" day
        desc = descColor $ text d
        comment = case c of {Nothing -> text ""; Just v -> text ";" <+> text v}
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

-- TODO this should probably go upstream
displayIO' :: Handle -> SimpleDoc -> IO ()
displayIO' handle simpleDoc
    = display simpleDoc
    where
      display SFail         = error $ "@SFail@ can not appear uncaught in a " ++
                              "rendered @SimpleDoc@"
      display SEmpty         = return ()
      display (SChar c x)    = do{ hPutChar handle c; display x}
      display (SText _ s x)  = do{ hPutStr handle s; display x}
      display (SLine i x)    = do{ hPutStr handle ('\n':indentation i); display x}
      display (SSGR s x)     = do
          supportsANSI <- hSupportsANSI handle
          if supportsANSI then
                          do{ hSetSGR handle s; display x}
                          else display x

indentation :: Int -> String
indentation = spaces

spaces :: Int -> String
spaces n        | n <= 0    = ""
                | otherwise = replicate n ' '

hPutDoc' :: Handle -> Doc -> IO ()
hPutDoc' handle doc = displayIO' handle (renderPretty 0.4 80 doc)

-- TODO this could use some refactoring
patternMatches :: Transaction -> Pattern -> Bool
patternMatches (Transaction _ _ obj _ _ _) (Pattern (Left Description) Is args) =
    obj `elem` args
patternMatches (Transaction _ _ obj _ _ _) (Pattern (Left Description) Matches args) =
    any (obj =~) args
patternMatches (Transaction _ _ _ obj _ _) (Pattern (Right (TwoWordObject Payer noun)) Matches args) =
    twoWordObjectMatches obj noun args
patternMatches (Transaction _ _ _ _ obj _) (Pattern (Right (TwoWordObject Payee noun)) Matches args) =
    twoWordObjectMatches obj noun args
patternMatches (Transaction _ _ _ obj _ _) (Pattern (Right (TwoWordObject Payer noun)) Is args) =
    twoWordObjectIs obj noun args
patternMatches (Transaction _ _ _ _ obj _) (Pattern (Right (TwoWordObject Payee noun)) Is args) =
    twoWordObjectIs obj noun args

twoWordObjectIs :: Posting -> Noun -> Arguments -> Bool
twoWordObjectIs (Posting acc _) Account args = acc `elem` args
twoWordObjectIs (Posting _ (Amount _ cur)) Currency args = cur `elem` args

twoWordObjectMatches :: Posting -> Noun -> Arguments -> Bool
twoWordObjectMatches (Posting acc _) Account args = any (acc =~) args
twoWordObjectMatches (Posting _ (Amount _ cur)) Currency args = any (cur =~) args

ruleMatches :: Transaction -> Rule -> Bool
ruleMatches t (Rule _ p _) = all (patternMatches t) p

executeAction :: Transaction -> Action -> Transaction
executeAction t (Action Set (Left Description) arg) = t {description = arg}
executeAction t
    (Action Set (Right (TwoWordObject Payer Account)) arg) =
        t {payerPosting = Posting arg amount}
            where
                (Posting _ amount) = payerPosting t
executeAction t
    (Action Set (Right (TwoWordObject Payee Account)) arg) =
        t {payeePosting = Posting arg amount}
            where
                (Posting _ amount) = payeePosting t
executeAction t
    (Action Set (Right (TwoWordObject Payer Currency)) arg) =
        t {payerPosting = Posting acc amount}
            where
                (Posting acc (Amount n _)) = payerPosting t
                amount = Amount n arg
executeAction t
    (Action Set (Right (TwoWordObject Payee Currency)) arg) =
        t {payeePosting = Posting acc amount}
            where
                (Posting acc (Amount n _)) = payeePosting t
                amount = Amount n arg

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
