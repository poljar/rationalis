{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Data.Time

import Data.Aeson
import Data.Aeson.Lens
import Data.Scientific

import Control.Lens
import Network.Wreq hiding (Options)
import System.IO
import Text.HTML.TagSoup hiding (parseOptions)
import Text.StringLike

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Network.Wreq.Session as S
import qualified Text.HTML.TagSoup as TS

fromPBZDate :: T.Text -> Day
fromPBZDate s =
    parseTimeOrError True defaultTimeLocale "%d.%m.%Y. %T" $ T.unpack s

decodeHTMLentities :: String -> String
decodeHTMLentities s = TS.fromTagText $ head $ TS.parseTags s

-- TODO ^?! aborts if it can't get the value
fromPBZ :: Data.Aeson.Lens.AsValue s => s -> Transactions
fromPBZ jsonData =
    jsonData ^.. members . key "result" . members .
    key "bankAccountTransactionList" .
    _Array .
    traverse .
    to
        (\t ->
             uncurry
                 (Transaction
                      ("PBZ-" ++
                       (t ^?! key "transactionNumber" . _String & T.unpack))
                      (t ^?! key "currencyDate" . _String & fromPBZDate)
                      (t ^?! key "description" . _String & T.unpack &
                       decodeHTMLentities))
                 (getPostings t))
  where
    getPostings t = fromMaybeAmounts (getAmounts t)
    getAmounts t =
        ( getAmount t "payAmount"
        , getAmount t "receiveAmount"
        , getAmount t "creditDebtAmount")
    getAmount t s = toMaybeAmount (amount t s) (currency t s)
    amount t s = t ^? key s . key "amount" . _Number & fmap toRealFloat
    currency t s = t ^? key s . key "currency" . key "currencyCode" . _String

toMaybeAmount :: Maybe Float -> Maybe T.Text -> Maybe Amount
toMaybeAmount (Just n) (Just s) = Just $ Amount n (T.unpack s)
toMaybeAmount Nothing _ = Nothing
toMaybeAmount _ Nothing = Nothing

fromMaybeAmounts ::
       (Maybe Amount, Maybe Amount, Maybe Amount) -> (Posting, Posting)
fromMaybeAmounts (Just (Amount n c), Nothing, Nothing) =
    ( Posting "Assets:PBZ" $ Amount (negate n) c
    , Posting "Expenses:???" $ Amount n c)
fromMaybeAmounts (Nothing, Just (Amount n1 c1), Just (Amount n2 c2)) =
    if c1 /= c2 -- this is probably a currency conversion
        then ( Posting "Assets:PBZ" $ Amount n1 c1
             , Posting "Assets:PBZ" $ Amount (negate n2) c2)
             -- normal income otherwise
        else ( Posting "Assets:PBZ" $ Amount n1 c1
             , Posting "Income:???" $ Amount (negate n1) c1)
fromMaybeAmounts (Nothing, Just (Amount n c), Nothing) =
    (Posting "Assets:PBZ" $ Amount n c, Posting "Income:???" $ Amount n c)
fromMaybeAmounts (Just a, Nothing, Just b) =
    if a == b
        then ( Posting "Assets:PBZ" $ Amount (negate n) c
             , Posting "Expenses:???" $ Amount n c)
        else ( Posting "Assets:???" $ Amount n c
             , Posting "Expenses:???" $ Amount 0.0 "Not implemented")
  where
    (Amount n c) = a

getCsrf :: (Show str, StringLike str) => Response str -> str
getCsrf r =
    fromAttrib "content" $ head $
    filter (~== TagOpen ("meta" :: String) [("name", "_csrf")]) $
    parseTags $
    r ^.
    responseBody

fetchPBZ :: Period -> IO B.ByteString
fetchPBZ period =
    S.withSession $ \sess -> do
        let userAgent =
                "Mozilla/5.0 (X11; Linux x86_64; rv:54.0) Gecko/20100101 Firefox/54.0"
        let acceptHeader =
                "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
        let encodingHeader = "identity"
        let domain = "net.pbz.hr"
        let path = "/pbz365/"
        let opts =
                defaults & header "User-Agent" .~ [userAgent] & header "Host" .~
                [domain] &
                header "Accept" .~
                [acceptHeader] &
                header "Accept-Encoding" .~
                [encodingHeader] &
                header "Connection" .~
                ["keep-alive"]
        r <- S.getWith opts sess "https://net.pbz.hr/pbz365/logonForm.htm"
        print $ r ^. responseCookie "JSESSIONID" . cookieValue
        let csrf = toString $ getCsrf r
        putStrLn csrf
        putStr "Card no: "
        hFlush stdout
        putStr "OTP: "
        hFlush stdout
        let pan = "xxx"
        let otp = "yyy"
        let postOpts =
                defaults & header "User-Agent" .~ [userAgent] & header "Host" .~
                [domain] &
                header "Accept" .~
                [acceptHeader] &
                header "Accept-Encoding" .~
                [encodingHeader] &
                header "Content-Type" .~
                ["application/x-www-form-urlencoded"] &
                header "Referer" .~
                ["https://net.pbz.hr/pbz365/logonForm.htm"]
        let login_data =
                [ "locale" := ("hr" :: String)
                , "serviceCode" := ("PBZ365@NET" :: String)
                , "authenticationType" := ("CAP" :: String)
                , "submitButton" := ("Potvrda" :: String)
                , "validUntil" := ("" :: String)
                , "pan" := (pan :: String)
                , "otp" := (otp :: String)
                , "_csfr" := (csrf :: String)
                ]
        let body = r ^?! responseBody
        return body

--        r <- S.getWith opts sess "https://httpbin.org/anything"
--        let csrf = toString ("a1a73d4e-40a9-4865-855d-1cef49159c3e" :: B.ByteString)
--        pan <- getLine
--        otp <- getLine
--        r <- S.postWith postOpts sess "https://net.pbz.hr/pbz365/app/logon" login_data
--        r <- S.postWith postOpts sess "https://httpbin.org/post" login_data
--        print r
--    S.getWith opts sess "https://net.pbz.hr/pbz365/app/logout"
main :: IO ()
main = do
    input <- getContents
    B.putStrLn $ encode (fromPBZ input)
