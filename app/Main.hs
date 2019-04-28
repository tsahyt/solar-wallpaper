{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.Time.Solar
import Data.Time (UTCTime (..), hoursToTimeZone)
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

-- | Run an Error String effect by dumping all errors into a 'Trace'
runErrorTrace :: (Member Trace r) => Sem (Error String ': r) () -> Sem r ()
runErrorTrace sem = do
    x <- runError sem
    case x of
        Left e -> trace $ "ERROR: " <> e
        Right () -> pure ()

-- | Run an output effect as a trace, accepting a conversion function.
runOutputAsPrettyTrace ::
       (Member Trace r) => (w -> String) -> Sem (Output w ': r) a -> Sem r a
runOutputAsPrettyTrace pr =
    interpret $ \case
        Output m -> trace $ pr m

-- | Run a time effect depending on whether time is given via CLI
runTimeCLI ::
       (Member (Lift IO) r, Member (Input CLI) r)
    => Sem (Time ': r) a
    -> Sem r a
runTimeCLI sem = do
    cli <- input
    case (unHelpful $ zone cli, unHelpful $ day cli) of
        (Nothing, Nothing) -> runTimeIO sem
        (Just z, Nothing) -> runTimeFixedZ (hoursToTimeZone z) sem
        (Nothing, Just t) -> runTimeFixedT (UTCTime t 0) sem
        (Just z, Just t) -> runTimePure (UTCTime t 0) (hoursToTimeZone z) sem

runMain ::
       ( Member FileSystem r
       , Member Trace r
       , Member Time r
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
    runFileSystemIO . runWallpaperIO . runTraceIO . runTimeCLI $
    runMain
