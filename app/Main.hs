{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib

import Data.Time
import Data.Time.Clock
import Data.Time.Calendar
import Data.Tuple
import Control.Monad
import Control.Monad.IO.Class
import Debug.Trace

import qualified Text.ParserCombinators.ReadP as RP

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

type Period = (Day, Day)

data Options = Options
    { period :: Period
    }

{-
periodReader :: ReadM Day
periodReader = eitherReader $ \arg ->
    case True of
      True -> Left ("Cannot parse date: " ++ arg)
      False -> Right (utctDay getCurrentTime)
-}

--getDay :: Day
getDay = fromGregorian 2017 07 18

periodParser :: [String] -> Maybe Period
periodParser args = Just (getDay, getDay)

monthParsers :: [RP.ReadP String]
monthParsers = map RP.string ["januar",
                              "jan",
                              "februar",
                              "feb",
                              "march",
                              "mar",
                              "may",
                              "jun",
                              "june",
                              "jul",
                              "july",
                              "aug",
                              "august",
                              "sep",
                              "september",
                              "oct",
                              "october",
                              "nov",
                              "november",
                              "dec",
                              "december"]

-- 2001-20-10
--
--
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
    RP.eof
    return $ fromGregorian year (fst rest) (snd rest)

-- "this|last year|month|quarter|day"
relativePeriods = map RP.string ["day", "month", "quarter", "year"]

thisSubstractor :: String -> Day -> Day
thisSubstractor period day = case period of
    "day"   -> day
    "month" -> day

relDatetoAbs :: String -> String -> Day
relDatetoAbs selector period = case selector of
    "this" -> fromGregorian year month day
        where (year, month, day)  = toGregorian $ getDay

-- last $month -> substract year
-- this $month -> do nothing
relDateParser :: RP.ReadP Day
relDateParser = do
    selector <- RP.string "this" <|> RP.string "last"
    period   <- RP.choice relativePeriods

    let today = getDay
    return $ fromGregorian 2001 10 10

dateParser :: RP.ReadP Day
dateParser = do
    year <- RP.string "2001"
    return $ fromGregorian (read year) 10 10

perdiodParserP :: RP.ReadP Period
perdiodParserP = do
    RP.skipSpaces
    _    <- RP.optional $ RP.string "from" <|> RP.string "since"
    RP.skipSpaces
    from <- RP.string "this" <|> RP.string "last" <|> RP.choice monthParsers
    RP.skipSpaces
    date <- numDateParser
    return (date, getDay)

parseMaybe :: RP.ReadP a -> String -> Maybe a
parseMaybe parser input =
    case RP.readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result

periodReader :: ReadM (Period)
periodReader = eitherReader $ \arg ->
    case periodParser $ words arg of
      Nothing -> Left $ trace(arg) $ ("Cannot parse date: " ++ arg)
      Just period -> Right period


--periodReader = eitherReader $ \arg ->
--    case parseTimeM True defaultTimeLocale "%b %d, %Y" arg of
--        Nothing -> Left ("Cannot parse date: " ++ arg)
--        Just day -> Right day

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
     <> progDesc "Fetch kurac palac"
     <> header "pbzfetch - a bank transaction fetcher" )

parser :: Options -> IO ()
parser (Options p) = putStrLn $ "Hello " ++ show p
