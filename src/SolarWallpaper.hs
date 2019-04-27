{-# LANGUAGE TupleSections #-}

module SolarWallpaper
    ( main'
    , imageSequence
    ) where

import Data.Time.Solar
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Time
import SolarWallpaper.Types

import qualified Data.Time as T

add24h :: ZonedTime -> ZonedTime
add24h t =
    ZonedTime (T.addLocalTime 86400 $ zonedTimeToLocalTime t) (zonedTimeZone t)

imageSequence :: Images -> ZonedTime -> Location -> (ZonedTime, [ImageBlock])
imageSequence imgs now here =
    let startTime = sunrise now here
        blocks =
            [ StaticImage (imgSunrise imgs) 3600
            , Transition
                  Overlay
                  (imgSunrise imgs)
                  (imgNoon imgs)
                  (T.zonedTimeToLocalTime (solarNoon now here) `T.diffLocalTime`
                   T.zonedTimeToLocalTime (sunrise now here) -
                   7200)
            , StaticImage (imgNoon imgs) 7200
            , Transition
                  Overlay
                  (imgNoon imgs)
                  (imgSunset imgs)
                  (T.zonedTimeToLocalTime (sunset now here) `T.diffLocalTime`
                   T.zonedTimeToLocalTime (solarNoon now here) -
                   7200)
            , StaticImage (imgSunset imgs) 3600
            , Transition
                  Overlay
                  (imgSunset imgs)
                  (imgEvening imgs)
                  ((T.zonedTimeToLocalTime (solarMidnight now here) `T.diffLocalTime`
                    T.zonedTimeToLocalTime (sunset now here) -
                    3600) /
                   2)
            , StaticImage (imgEvening imgs) 3600
            , Transition
                  Overlay
                  (imgEvening imgs)
                  (imgMidnight imgs)
                  ((T.zonedTimeToLocalTime (solarMidnight now here) `T.diffLocalTime`
                    T.zonedTimeToLocalTime (sunset now here) -
                    3600) /
                   2)
            , StaticImage (imgMidnight imgs) 7200
            , Transition
                  Overlay
                  (imgMidnight imgs)
                  (imgSunrise imgs)
                  ((T.zonedTimeToLocalTime (add24h $ sunrise now here) `T.diffLocalTime`
                    T.zonedTimeToLocalTime (solarMidnight now here) -
                    7200))
            ]
     in (startTime, blocks)

main' ::
       ( Member Time r
       , Member (Input Images) r
       , Member (Input Location) r
       , Member (Output (ZonedTime, [ImageBlock])) r
       )
    => Sem r ()
main' = do
    now <- getZonedTime
    imgs <- input
    here <- input
    output $ imageSequence imgs now here
