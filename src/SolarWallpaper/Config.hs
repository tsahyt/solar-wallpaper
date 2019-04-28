{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module SolarWallpaper.Config (
    Config (..),
    readConfig,
    runInputFromCLI,
    CLI (..)
) where

import Polysemy
import Polysemy.FileSystem
import Polysemy.Error
import Polysemy.Input
import SolarWallpaper.Types
import GHC.Generics
import Data.Time.Solar (Location(..))
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Options.Generic

import Prelude hiding (readFile)

import Toml (TomlCodec, (.=))
import qualified Toml

data Config = Config
    { configLocation :: !Location 
    , configImages :: !Images
    , configOutput :: !FilePath }

tomlImages :: TomlCodec Images
tomlImages =
    Images 
        <$> Toml.string "sunrise" .= imgSunrise
        <*> Toml.string "noon" .= imgNoon
        <*> Toml.string "sunset"  .= imgSunset
        <*> Toml.string "evening" .= imgEvening
        <*> Toml.string "midnight" .= imgMidnight

tomlLocation :: TomlCodec Location
tomlLocation =
    Location
        <$> Toml.double "latitude" .= latitude
        <*> Toml.double "longitude" .= longitude

tomlConfig :: TomlCodec Config
tomlConfig =
    Config 
        <$> Toml.table tomlLocation "loc" .= configLocation 
        <*> Toml.table tomlImages "img" .= configImages 
        <*> Toml.string "out.path" .= configOutput

readConfig ::
       (Member FileSystem r, Member (Error String) r)
    => FilePath
    -> Sem r Config
readConfig path = do
    exists <- doesFileExist path
    if not exists
        then throw @String "Config file not found!"
        else do
            raw <- decodeUtf8 <$> readFile path
            case Toml.decode tomlConfig raw of
                Left e -> throw (unpack $ Toml.prettyException e)
                Right x -> pure x

data CLI = CLI
    { configPath :: FilePath <?> "Path to configuration file"
    , apply :: Bool <?> "Apply Wallpaper after writing"
    }
    deriving (Generic)

instance ParseRecord CLI where
    parseRecord =
        parseRecordWithModifiers $
        defaultModifiers {shortNameModifier = firstLetter}

runInputFromCLI ::
       forall i r a. (ParseRecord i, Member (Lift IO) r)
    => Sem (Input i ': r) a
    -> Sem r a
runInputFromCLI sem = do
    i <- sendM (getRecord "foo" :: IO i)
    interpret
        (\case
             Input -> pure i)
        sem
