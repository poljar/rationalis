{-# LANGUAGE OverloadedStrings #-}
module PBZ
    ( fetchPBZ
    , fromPBZ
    ) where

import Lib

import Data.Time

import Data.Aeson.Lens
import Data.Scientific

import System.IO
import Control.Lens
import Text.StringLike
import Text.HTML.TagSoup hiding (parseOptions)
import Network.Wreq hiding (Options)

import qualified Data.Text as T
import qualified Text.HTML.TagSoup as TS
import qualified Network.Wreq.Session as S
import qualified Data.ByteString.Lazy as B

fromPBZDate :: T.Text -> Day
fromPBZDate s = parseTimeOrError True defaultTimeLocale "%d.%m.%Y. %T" $ T.unpack s

decodeHTMLentities :: String -> String
decodeHTMLentities s = TS.fromTagText $ head $ TS.parseTags s

-- TODO ^?! aborts if it can't get the value
fromPBZ :: Data.Aeson.Lens.AsValue s => s -> Transactions
fromPBZ jsonData = jsonData ^.. members . key "result" . members .
    key "bankAccountTransactionList" . _Array .
    traverse . to (\t -> Transaction
        ( "PBZ-" ++ (t ^?! key "transactionNumber" . _String & T.unpack))
        ( t ^?! key "currencyDate" . _String & fromPBZDate)
        ( t ^?! key "description" . _String & T.unpack & decodeHTMLentities)
        ( t ^?  key "payAmount" . key "amount" . _Number & fmap toRealFloat)
        ( t ^?  key "receiveAmount" . key "amount" . _Number & fmap toRealFloat)
        ( t ^?! key "amountAfterTransaction" . key "currency" .
                key "currencyCode" . _String & T.unpack)
        )

getCsrf :: (Show str, StringLike str) => Response str -> str
getCsrf r = fromAttrib "content" $ head $
            filter (~== TagOpen ("meta" :: String) [("name", "_csrf")]) $
            parseTags $ r ^. responseBody

fetchPBZ :: Period -> IO B.ByteString
fetchPBZ period = S.withSession $ \sess -> do
    let userAgent      = "Mozilla/5.0 (X11; Linux x86_64; rv:54.0) Gecko/20100101 Firefox/54.0"
    let acceptHeader   = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
    let encodingHeader = "identity"
    let domain         = "net.pbz.hr"
    let path           = "/pbz365/"

    let opts = defaults & header "User-Agent"      .~ [userAgent]
                        & header "Host"            .~ [domain]
                        & header "Accept"          .~ [acceptHeader]
                        & header "Accept-Encoding" .~ [encodingHeader]
                        & header "Connection"      .~ ["keep-alive"]

    r <- S.getWith opts sess "https://net.pbz.hr/pbz365/logonForm.htm"
    print $ r ^. responseCookie "JSESSIONID" . cookieValue
--        r <- S.getWith opts sess "https://httpbin.org/anything"

--        let csrf = toString ("a1a73d4e-40a9-4865-855d-1cef49159c3e" :: B.ByteString)
    let csrf = toString $ getCsrf r
    putStrLn csrf
    putStr "Card no: "
    hFlush stdout
--        pan <- getLine
    putStr "OTP: "
    hFlush stdout
--        otp <- getLine
    let pan = "xxx"
    let otp = "yyy"

    let postOpts = defaults & header "User-Agent"      .~ [userAgent]
                            & header "Host"            .~ [domain]
                            & header "Accept"          .~ [acceptHeader]
                            & header "Accept-Encoding" .~ [encodingHeader]
                            & header "Content-Type"    .~ ["application/x-www-form-urlencoded"]
                            & header "Referer"         .~ ["https://net.pbz.hr/pbz365/logonForm.htm"]

    let login_data = [ "locale"             := ( "hr"         :: String )
                     , "serviceCode"        := ( "PBZ365@NET" :: String )
                     , "authenticationType" := ( "CAP"        :: String )
                     , "submitButton"       := ( "Potvrda"    :: String )
                     , "validUntil"         := ( ""           :: String )
                     , "pan"                := ( pan          :: String )
                     , "otp"                := ( otp          :: String )
                     , "_csfr"              := ( csrf         :: String )
                     ]
--        r <- S.postWith postOpts sess "https://net.pbz.hr/pbz365/app/logon" login_data
--        r <- S.postWith postOpts sess "https://httpbin.org/post" login_data
--        print r

    let body = r ^?! responseBody

--    S.getWith opts sess "https://net.pbz.hr/pbz365/app/logout"
    return body
