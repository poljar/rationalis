{-# LANGUAGE OverloadedStrings #-}
module Config
    where

import Data.ConfigFile
import Control.Monad.Except

readConf = do
    rv <- runExceptT $
        do
        cp <- join $ liftIO $ readfile emptyCP "./test.conf"
        let x = cp
        liftIO $ putStrLn "In the test"
        nb <- get x "DEFAULT" "hostname"
        liftIO $ putStrLn nb
        let foo = sections x
        liftIO $ print foo
        return "done"
    print rv
