{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Teletype
    ( Teletype(..)
    , putError
    , runTeletypeIO
    ) where

import Polysemy
import System.IO

data Teletype m a where
    PutError :: String -> Teletype m ()

makeSem ''Teletype

runTeletypeIO :: Member (Lift IO) r => Sem (Teletype ': r) a -> Sem r a
runTeletypeIO =
    interpret $ \case
        PutError e -> sendM $ hPutStrLn stderr e
