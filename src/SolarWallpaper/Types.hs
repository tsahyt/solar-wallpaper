module SolarWallpaper.Types (
    Images(..),
    absolutifyImages,
    TransitionType(..),
    ImageBlock(..),
    blockTime,
    blockImage,
    addLocalTime,
    diffLocalTime,
    addZonedTime
) where

import Polysemy
import Polysemy.FileSystem
import System.FilePath (takeDirectory)
import Data.Time (NominalDiffTime)
import qualified Data.Time as T

data Images = Images
    { imgSunrise :: !FilePath
    , imgNoon :: !FilePath
    , imgSunset :: !FilePath
    , imgEvening :: !FilePath
    , imgMidnight :: !FilePath
    } deriving (Show, Eq, Ord, Read)

absolutifyImages :: Member FileSystem r => FilePath -> Images -> Sem r Images
absolutifyImages inpPath images =
    changeDir (takeDirectory inpPath) *>
    (Images <$> makeAbsolute (imgSunrise images) <*>
     makeAbsolute (imgNoon images) <*>
     makeAbsolute (imgSunset images) <*>
     makeAbsolute (imgEvening images) <*>
     makeAbsolute (imgMidnight images))

data TransitionType =
    Overlay
    deriving (Show, Eq, Ord, Read)

data ImageBlock
    = StaticImage FilePath
                  NominalDiffTime
    | Transition TransitionType
                 FilePath
                 FilePath
                 NominalDiffTime
    deriving (Show, Eq, Ord)

blockTime :: ImageBlock -> NominalDiffTime
blockTime (StaticImage _ t) = t
blockTime (Transition _ _ _ t) = t

-- | Return the last image of the block
blockImage :: ImageBlock -> FilePath
blockImage (StaticImage p _) = p
blockImage (Transition _ _ p _) = p

-- | Lifted from time-1.9
addLocalTime :: T.NominalDiffTime -> T.LocalTime -> T.LocalTime
addLocalTime x = T.utcToLocalTime T.utc . T.addUTCTime x . T.localTimeToUTC T.utc

-- | Lifted from time-1.9
diffLocalTime :: T.LocalTime -> T.LocalTime -> T.NominalDiffTime
diffLocalTime a b = T.diffUTCTime (T.localTimeToUTC T.utc a) (T.localTimeToUTC T.utc b)

addZonedTime :: T.NominalDiffTime -> T.ZonedTime -> T.ZonedTime
addZonedTime t (T.ZonedTime x z) = T.ZonedTime (addLocalTime t x) z
