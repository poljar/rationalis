{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Config
    ( readConf
    , readAccount
    , confErrorPretty
    , Accounts
    , Account(..)
    , Config(..)
    ) where

import Control.Monad.Except
import Data.ConfigFile

newtype Config = Config
    { accounts :: Accounts
    } deriving (Show)

instance Monoid Config where
    mempty = Config []
    mappend (Config a) (Config b) = Config (a ++ b)

type Accounts = [Account]

data Account = Account
    { accountName :: String
    , fetcher :: FilePath
    , userName :: String
    , outDir :: FilePath
    } deriving (Show)

readAccount :: MonadError CPError m => ConfigParser -> SectionSpec -> m Account
readAccount cp s = do
    f <- get cp s "fetcher"
    u <- get cp s "user"
    o <- get cp s "ledger-dir"
    return (Account s f u o)

readAccounts ::
       MonadError CPError m => ConfigParser -> [SectionSpec] -> m Accounts
readAccounts cp = mapM (readAccount cp)

-- TODO better error messages
confErrorPretty err = "Error parsing conf: " ++ show err

readConf :: MonadIO m => FilePath -> m (Either CPError Config)
readConf file =
    runExceptT $ do
        cp <- join $ liftIO $ readfile emptyCP file
        let s = sections cp
        a <- readAccounts cp s
        return (Config a)
