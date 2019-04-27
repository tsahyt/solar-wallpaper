module Main where

import Polysemy
import Polysemy.Time
import Polysemy.Input
import Polysemy.Error
import Polysemy.FileSystem
import Polysemy.Teletype
import Data.Time.Solar
import SolarWallpaper
import SolarWallpaper.Types
import SolarWallpaper.XML
import SolarWallpaper.Config
import Options.Generic (unHelpful)

runMain ::
       ( Member Time r
       , Member FileSystem r
       , Member Teletype r
       , Member (Input CLI) r
       )
    => Sem r ()
runMain = do
    res <- runError @String $ do
        conf <- do
            cli <- input
            readConfig (unHelpful $ configPath cli)
        runConstInput @Images (configImages conf) . 
            runConstInput @Location (configLocation conf) .
            outputToFile (configOutput conf) (uncurry xmlImageBlocks)
                $ main'
    case res of
        Left e -> putError e
        Right () -> pure ()

main :: IO ()
main =
    runM .
    runInputFromCLI @CLI .
    runFileSystemIO .
    runTeletypeIO .
    runTimeIO $
    runMain
