{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( Period
    , Password
    , getJSON
    , tryGetFile
    , hPutDoc'
    ) where

import Control.Exception

import Data.Time

import System.Console.ANSI (hSetSGR, hSupportsANSI)
import System.IO
import Text.PrettyPrint.ANSI.Leijen

import qualified Data.ByteString.Lazy as B

type Password = String

type Period = (Day, Day)

-- TODO this should probably go upstream
displayIO' :: Handle -> SimpleDoc -> IO ()
displayIO' handle = display
  where
    display SFail =
        error $
        "@SFail@ can not appear uncaught in a " ++ "rendered @SimpleDoc@"
    display SEmpty = return ()
    display (SChar c x) = do
        hPutChar handle c
        display x
    display (SText _ s x) = do
        hPutStr handle s
        display x
    display (SLine i x) = do
        hPutStr handle ('\n' : indentation i)
        display x
    display (SSGR s x) = do
        supportsANSI <- hSupportsANSI handle
        if supportsANSI
            then do
                hSetSGR handle s
                display x
            else display x

indentation :: Int -> String
indentation = spaces

spaces :: Int -> String
spaces n
    | n <= 0 = ""
    | otherwise = replicate n ' '

hPutDoc' :: Handle -> Doc -> IO ()
hPutDoc' handle doc = displayIO' handle (renderPretty 0.4 80 doc)

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
