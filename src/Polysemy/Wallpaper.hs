{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Polysemy.Wallpaper
    ( Wallpaper(..)
    , setWallpaper
    , runWallpaperIO
    , runWallpaperIgnore
    ) where

import Polysemy

import Control.Monad
import Data.Text (pack)
import qualified GI.Gio as Gio

data Wallpaper m a where
    SetWallpaper :: FilePath -> Wallpaper m ()

makeSem ''Wallpaper

makeSettings :: IO Gio.Settings
makeSettings = Gio.settingsNew "org.gnome.desktop.background"

runWallpaperIO :: Member (Lift IO) r => Sem (Wallpaper ': r) a -> Sem r a
runWallpaperIO sem = do
    gsettings <- sendM makeSettings
    interpret
        (\case
             SetWallpaper path ->
                 sendM @IO $
                 void $
                 Gio.settingsSetString
                     gsettings
                     "picture-uri"
                     ("file://" <> pack path))
        sem

runWallpaperIgnore :: Sem (Wallpaper ': r) a -> Sem r a
runWallpaperIgnore =
    interpret $ \case
        SetWallpaper _ -> pure ()
