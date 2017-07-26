{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( parseMaybe
    , periodParser
    , Period
    , Transaction
    , fromPBZ
    , printTransaction
    ) where

import Data.Maybe
import Data.Aeson.Lens
import Data.Scientific

import Data.Time
import Data.Time.Clock
import Data.Time.Calendar

import Control.Lens
import Control.Applicative
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Text.ParserCombinators.ReadP as RP

type Period = (Day, Day)

digit :: RP.ReadP Char
digit =
    RP.satisfy (\char -> char >= '0' && char <= '9')

numbers :: Int -> RP.ReadP Integer
numbers digits =
    fmap read (RP.count digits digit)

dateSeparator :: RP.ReadP ()
dateSeparator = do
    RP.char '-' <|> RP.char '/'
    return ()

mdNumberParser :: RP.ReadP Int
mdNumberParser = do
    dateSeparator
    fmap fromInteger $ numbers 2 <|> numbers 1

monthDayParser :: RP.ReadP (Int, Int)
monthDayParser = do
    month <- mdNumberParser
    day   <- RP.option 1 mdNumberParser
    if month < 1 || month > 12 || day < 1 || day > 31 then
        RP.pfail
    else
        return (month, day)

numDateParser :: RP.ReadP Day
numDateParser = do
    year <- numbers 4
    rest <- RP.option (1, 1) monthDayParser
    return $ fromGregorian year (fst rest) (snd rest)


-- TODO Implement relative dates e.g. last year
-- TODO until date should be optional and set to today by default
periodParser :: RP.ReadP Period
periodParser = do
    RP.skipSpaces
    _    <- RP.optional $ RP.string "from" <|> RP.string "since"
    RP.skipSpaces
    since <- numDateParser
    RP.skipSpaces
    _    <- RP.optional $ RP.string "to" <|> RP.string "until"
    RP.skipSpaces
    until <- numDateParser
    RP.eof
    return (since, until)

parseMaybe :: RP.ReadP a -> String -> Maybe a
parseMaybe parser input =
    case RP.readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result


data Transaction = Transaction
    { id          :: String
    , date        :: Day
    , description :: String
    , payAmount   :: Maybe Float
    , recAmount   :: Maybe Float
    , currency    :: String
    } deriving (Show)

-- TODO indentation should be based on the lengths of the payer/payee accounts
-- some pretty printing lib pretty please?
-- TODO replace ??? using regex based rules
printTransaction :: Transaction -> IO ()
printTransaction (Transaction id day description pay rec currency) = do
    putStrLn descriptionLine
    putStrLn payeeLine
    putStrLn payerLine
    putStrLn ""
        where descriptionLine = d ++ " * " ++ description
              d = formatTime defaultTimeLocale "%Y/%m/%d" day
              payeeLine = indent ++ "Expenses:???" ++ indent ++ (show amount)
                          ++ " " ++ currency ++ " ; id: " ++ id
              payerLine = indent ++ "Assets:PBZ" ++ indent ++ " -" ++ (show amount)
                          ++ " " ++ currency
              indent = "    "
              amount = case pay of
                    Just pay -> pay
                    Nothing  -> case rec of
                        Just rec -> rec
                        Nothing  -> 0.0

parseDate :: T.Text -> Day
parseDate s = parseTimeOrError True defaultTimeLocale "%d.%m.%Y. %T" $ T.unpack s

-- TODO ^?! aborts if it can't get the value
fromPBZ :: Data.Aeson.Lens.AsValue s => s -> [Transaction]
fromPBZ jsonData = jsonData ^.. members . key "result" . members .
    key "bankAccountTransactionList" . _Array .
    traverse . to (\t -> Transaction
        ( "PBZ-" ++ (t ^?! key "transactionNumber" . _String & T.unpack))
        ( t ^?! key "currencyDate" . _String & parseDate)
        ( t ^?! key "description" . _String & T.unpack)
        ( t ^?  key "payAmount" . key "amount" . _Number & fmap toRealFloat)
        ( t ^?  key "receiveAmount" . key "amount" . _Number & fmap toRealFloat)
        ( t ^?! key "amountAfterTransaction" . key "currency" .
                key "currencyCode" . _String & T.unpack)
        )
