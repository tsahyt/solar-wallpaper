{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module SolarWallpaper.XML
    ( xmlImageBlocks
    , imageBlock
    , startTime
    ) where

import Data.ByteString.Char8 (ByteString, pack)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import SolarWallpaper.Types
import Text.Printf
import Text.XML.Light

showDuration :: NominalDiffTime -> String
showDuration t =
    let it = truncate t :: Integer
        dt = fromIntegral it :: Double
     in printf "%.1f" dt

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
