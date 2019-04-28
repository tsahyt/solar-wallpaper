{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.Time.Solar
import Options.Generic (unHelpful)
import Polysemy
import Polysemy.Error
import Polysemy.FileSystem
import Polysemy.Input
import Polysemy.Time
import Polysemy.Trace
import Polysemy.Wallpaper
import Polysemy.Output
import SolarWallpaper
import SolarWallpaper.Config
import SolarWallpaper.Types
import SolarWallpaper.XML
import SolarWallpaper.Pretty

runErrorTrace :: (Member Trace r) => Sem (Error String ': r) () -> Sem r ()
runErrorTrace sem = do
    x <- runError sem
    case x of
        Left e -> trace $ "ERROR: " <> e
        Right () -> pure ()

runOutputAsPrettyTrace ::
       (Member Trace r) => (w -> String) -> Sem (Output w ': r) a -> Sem r a
runOutputAsPrettyTrace pr =
    interpret $ \case
        Output m -> trace $ pr m

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
        conf <- readConfig (unHelpful $ inputPath cli)
        runConstInput @Images (configImages conf) .
            runConstInput @Location (configLocation conf) $
            case cli of
                Generate {} -> do
                    outputToFile (configOutput conf) (uncurry xmlImageBlocks) $
                        main'
                    when (unHelpful $ apply cli) $
                        setWallpaper (configOutput conf)
                Times {} -> runOutputAsPrettyTrace (uncurry prettyOut) $ main'

main :: IO ()
main =
    runM .
    runInputFromCLI @CLI "Solar Wallpaper Generator for GNOME" .
    runFileSystemIO . runWallpaperIO . runTraceIO . runTimeIO $
    runMain
