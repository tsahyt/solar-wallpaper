{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Time
    ( Time(..)
    , getCurrentTime
    , getCurrentTimeZone
    , getZonedTime
    , utcToLocalZoned
    -- * Interpreters
    , runTimeIO
    , runTimeFixedT
    , runTimeFixedZ
    , runTimePure
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
runTimeIO =
    interpret $ \case
        GetCurrentTime -> sendM T.getCurrentTime
        GetCurrentTimeZone -> sendM T.getCurrentTimeZone

-- | Interpret a 'Time' effect at a fixed time, but with the local timezone.
runTimeFixedT :: Member (Lift IO) r => T.UTCTime -> Sem (Time ': r) a -> Sem r a
runTimeFixedT t =
    interpret $ \case
        GetCurrentTime -> pure t
        GetCurrentTimeZone -> sendM T.getCurrentTimeZone

-- | Interpret a 'Time' effect at a fixed time, but with the local timezone.
runTimeFixedZ ::
       Member (Lift IO) r => T.TimeZone -> Sem (Time ': r) a -> Sem r a
runTimeFixedZ zone =
    interpret $ \case
        GetCurrentTime -> sendM T.getCurrentTime
        GetCurrentTimeZone -> pure zone

runTimePure :: T.UTCTime -> T.TimeZone -> Sem (Time ': r) a -> Sem r a
runTimePure t zone =
    interpret $ \case
        GetCurrentTime -> pure t
        GetCurrentTimeZone -> pure zone
