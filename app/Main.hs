module Main where

import Polysemy
import Polysemy.Time
import Polysemy.Input
import Polysemy.FileSystem
import Data.Time.Solar
import SolarWallpaper
import SolarWallpaper.Types
import SolarWallpaper.XML

main :: IO ()
main =
    runM .
    runFileSystemIO .
    runTimeIO .
    outputToFile "/tmp/out" (uncurry xmlImageBlocks) .
    runConstInput @Images (Images "sunrise" "noon" "sunset" "evening" "midnight") $
    runConstInput @Location (Location 46 13) $
    main'
