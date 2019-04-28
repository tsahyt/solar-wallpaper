{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module SolarWallpaper.Config (
    SolarInput (..),
    readInputFile,
    runInputFromCLI,
    CLI (..)
) where

import Polysemy
import Polysemy.FileSystem
import Polysemy.Error
import Polysemy.Input
import SolarWallpaper.Types
import GHC.Generics
import Data.Time (Day)
import Data.Time.Solar (Location(..))
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Options.Generic

import Prelude hiding (readFile)

import Toml (TomlCodec, (.=))
import qualified Toml

data SolarInput = SolarInput
    { solarLocation :: !Location 
    , solarImages :: !Images
    , solarOutput :: !FilePath
    , solarBias :: Maybe Double }

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

tomlConfig :: TomlCodec SolarInput
tomlConfig =
    SolarInput
        <$> Toml.table tomlLocation "loc" .= solarLocation 
        <*> Toml.table tomlImages "img" .= solarImages 
        <*> Toml.string "out.path" .= solarOutput
        <*> Toml.dioptional (Toml.double "bias") .= solarBias

readInputFile ::
       (Member FileSystem r, Member (Error String) r)
    => FilePath
    -> Sem r SolarInput
readInputFile path = do
    exists <- doesFileExist path
    if not exists
        then throw @String "Config file not found!"
        else do
            raw <- decodeUtf8 <$> readFile path
            case Toml.decode tomlConfig raw of
                Left e -> throw (unpack $ Toml.prettyException e)
                Right x ->
                    SolarInput <$> pure (solarLocation x) <*>
                    absolutifyImages path (solarImages x) <*>
                    pure (solarOutput x) <*>
                    pure (solarBias x)

data CLI
    = Generate { inputPath :: FilePath <?> "Path to input file"
               , apply :: Bool <?> "Apply Wallpaper after writing"
               , day :: Maybe Day <?> "Day to use instead of current date"
               , zone :: Maybe Int <?> "Timezone to use instead of local" }
    | Times { inputPath :: FilePath <?> "Path to input file"
            , day :: Maybe Day <?> "Day to use instead of current date"
            , zone :: Maybe Int <?> "Timezone to use instead of local" }
    deriving (Generic)

instance ParseRecord CLI where
    parseRecord =
        parseRecordWithModifiers $
        defaultModifiers {shortNameModifier = firstLetter}

runInputFromCLI ::
       forall i r a. (ParseRecord i, Member (Lift IO) r)
    => Text
    -> Sem (Input i ': r) a
    -> Sem r a
runInputFromCLI desc sem = do
    i <- sendM (getRecord desc :: IO i)
    interpret
        (\case
             Input -> pure i)
        sem
