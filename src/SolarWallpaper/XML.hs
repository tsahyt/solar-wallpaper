{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module SolarWallpaper.XML
    ( xmlImageBlocks
    , imageBlock
    , startTime
    ) where

import Data.ByteString.Char8 (ByteString, pack)
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Time.Clock
import SolarWallpaper.Types
import Text.XML.Light
import Text.Printf

showDuration :: NominalDiffTime -> String
showDuration =
    printf "%.1f" .
    (id :: Double -> Double) . realToFrac . nominalDiffTimeToSeconds

showDoubleDigit :: (PrintfArg a, Integral a) => a -> String
showDoubleDigit = printf "%0.2d"

imageBlock :: ImageBlock -> Element
imageBlock (StaticImage p d) =
    unode "static" [unode "duration" (showDuration d), unode "file" p]
imageBlock (Transition ty from to d) =
    Attr (unqual "type") (transitionType ty) `add_attr`
    unode
        "transition"
        [unode "duration" (showDuration d), unode "from" from, unode "to" to]

startTime :: ZonedTime -> Element
startTime (ZonedTime t _) =
    unode
        "starttime"
        [ unode "year" $ show year
        , unode "month" $ showDoubleDigit month
        , unode "day" $ showDoubleDigit day
        , unode "hour" . showDoubleDigit . todHour $ tod
        , unode "minute" . showDoubleDigit . todMin $ tod
        , unode "second" . showDoubleDigit @Int . truncate . todSec $ tod
        ]
  where
    (year, month, day) = toGregorian (localDay t)
    tod = localTimeOfDay t

transitionType :: TransitionType -> String
transitionType Overlay = "overlay"

xmlImageBlocks :: ZonedTime -> [ImageBlock] -> ByteString
xmlImageBlocks t blks =
    pack . ppTopElement $ unode "background" (startTime t : map imageBlock blks)
