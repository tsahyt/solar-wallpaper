module Main where

import Control.Monad
import Polysemy
import Polysemy.Time
import Polysemy.Input
import Polysemy.Error
import Polysemy.FileSystem
import Polysemy.Trace
import Polysemy.Wallpaper
import Data.Time.Solar
import SolarWallpaper
import SolarWallpaper.Types
import SolarWallpaper.XML
import SolarWallpaper.Config
import Options.Generic (unHelpful)

runErrorTrace :: (Member Trace r) => Sem (Error String ': r) () -> Sem r ()
runErrorTrace sem = do
    x <- runError sem
    case x of
        Left e -> trace $ "ERROR: " <> e
        Right () -> pure ()

runMain ::
       ( Member Time r
       , Member FileSystem r
       , Member Trace r
       , Member Wallpaper r
       , Member (Input CLI) r
       )
    => Sem r ()
runMain =
    runErrorTrace $ do
        cli <- input
        conf <- readConfig (unHelpful $ configPath cli)
        runConstInput @Images (configImages conf) . 
            runConstInput @Location (configLocation conf) .
            outputToFile (configOutput conf) (uncurry xmlImageBlocks)
                $ main'
        when (unHelpful $ apply cli) $ 
            setWallpaper (configOutput conf)

main :: IO ()
main =
    runM .
    runInputFromCLI @CLI .
    runFileSystemIO .
    runWallpaperIO .
    runTraceIO .
    runTimeIO $
    runMain
