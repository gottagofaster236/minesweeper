{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Server (app, runWebM)
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Default.Class
import Prelude ()
import Prelude.Compat
import Web.Scotty.Trans

main :: IO ()
main = do
    sync <- newTVarIO def
    let runActionToIO m = runReaderT (runWebM m) sync
    putStrLn "Starting server on port 3000"
    scottyT 3000 runActionToIO app
