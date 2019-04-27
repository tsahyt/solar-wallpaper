{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Time
    ( Time(..)
    , getCurrentTime
    , getCurrentTimeZone
    , getZonedTime
    , utcToLocalZoned
    -- * Interpreters
    , runTimeIO
    , runTimeFixed
    ) where

import qualified Data.Time as T
import Polysemy

data Time m a where
    GetCurrentTime :: Time m T.UTCTime
    GetCurrentTimeZone :: Time m T.TimeZone

makeSem ''Time

getZonedTime :: Member Time r => Sem r T.ZonedTime
getZonedTime = do
    now <- getCurrentTime
    zone <- getCurrentTimeZone
    pure $ T.utcToZonedTime zone now

utcToLocalZoned :: Member Time r => T.UTCTime -> Sem r T.ZonedTime
utcToLocalZoned t = do
    zone <- getCurrentTimeZone
    pure $ T.utcToZonedTime zone t

runTimeIO :: Member (Lift IO) r => Sem (Time ': r) a -> Sem r a
runTimeIO = interpret $ \case
    GetCurrentTime -> sendM T.getCurrentTime
    GetCurrentTimeZone -> sendM T.getCurrentTimeZone

runTimeFixed :: T.UTCTime -> T.TimeZone -> Sem (Time ': r) a -> Sem r a
runTimeFixed t zone = interpret $ \case
    GetCurrentTime -> pure t
    GetCurrentTimeZone -> pure zone
