{-# LANGUAGE TupleSections #-}

module SolarWallpaper
    ( main'
    , imageSequence
    , traceTimes
    -- * Time Utility Functions
    , diffLocalTime
    ) where

import Data.Time.Solar
import Data.Maybe (fromMaybe)
import Data.Foldable (traverse_)
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Trace
import Polysemy.Time
import SolarWallpaper.Types
import SolarWallpaper.Config

import qualified Data.Time as T

add24h :: ZonedTime -> ZonedTime
add24h t =
    ZonedTime (addLocalTime 86400 $ zonedTimeToLocalTime t) (zonedTimeZone t)

imageSequence :: Images -> ZonedTime -> Location -> Maybe Double -> (ZonedTime, [ImageBlock])
imageSequence imgs now here bias =
    let startTime = sunrise now here
        bias' = fromMaybe 0.5 bias
        blocks =
            [ StaticImage (imgSunrise imgs) 3600
            , Transition
                  Overlay
                  (imgSunrise imgs)
                  (imgNoon imgs)
                  (T.zonedTimeToLocalTime (solarNoon now here) `diffLocalTime`
                   T.zonedTimeToLocalTime (sunrise now here) -
                   7200)
            , StaticImage (imgNoon imgs) 7200
            , Transition
                  Overlay
                  (imgNoon imgs)
                  (imgSunset imgs)
                  (T.zonedTimeToLocalTime (sunset now here) `diffLocalTime`
                   T.zonedTimeToLocalTime (solarNoon now here) -
                   7200 - 3600)
            , StaticImage (imgSunset imgs) 3600
            , Transition
                  Overlay
                  (imgSunset imgs)
                  (imgEvening imgs)
                  ((T.zonedTimeToLocalTime (solarMidnight now here) `diffLocalTime`
                    T.zonedTimeToLocalTime (sunset now here)) * realToFrac bias')
            , StaticImage (imgEvening imgs) 3600
            , Transition
                  Overlay
                  (imgEvening imgs)
                  (imgMidnight imgs)
                  ((T.zonedTimeToLocalTime (solarMidnight now here) `diffLocalTime`
                    T.zonedTimeToLocalTime (sunset now here)) * realToFrac (1 - bias'))
            , StaticImage (imgMidnight imgs) 7200
            , Transition
                  Overlay
                  (imgMidnight imgs)
                  (imgSunrise imgs)
                  ((T.zonedTimeToLocalTime (add24h $ sunrise now here) `diffLocalTime`
                    T.zonedTimeToLocalTime (solarMidnight now here) -
                    7200))
            ]
     in (startTime, blocks)

traceTimes :: (Member Trace r, Member Time r, Member (Input SolarInput) r) => Sem r ()
traceTimes = do
    now <- getZonedTime
    inp <- input
    let loc = solarLocation inp
    traverse_ trace
        [ "Sunrise: " <> show (sunrise now loc)
        , "Solar Noon: " <> show (solarNoon now loc)
        , "Sunset: " <> show (sunset now loc)
        , "Solar Midnight: " <> show (solarMidnight now loc)
        ]

main' ::
       ( Member Time r
       , Member (Input SolarInput) r
       , Member (Output (ZonedTime, [ImageBlock])) r
       )
    => Sem r ()
main' = do
    now <- getZonedTime
    inp <- input
    output $ imageSequence (solarImages inp) now (solarLocation inp) (solarBias inp)
