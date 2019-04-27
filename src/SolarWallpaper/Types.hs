module SolarWallpaper.Types (
    Images(..),
    TransitionType(..),
    ImageBlock(..),
    blockTime,
    blockImage
) where

import Data.Time (NominalDiffTime)

data Images = Images
    { imgSunrise :: FilePath
    , imgNoon :: FilePath
    , imgSunset :: FilePath
    , imgEvening :: FilePath
    , imgMidnight :: FilePath
    } deriving (Show, Eq, Ord, Read)

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
