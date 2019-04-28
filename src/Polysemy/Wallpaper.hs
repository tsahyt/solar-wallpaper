{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Wallpaper
    ( Wallpaper(..)
    , setWallpaper
    ) where

import Polysemy

-- import qualified GI.Gio as Gio

data Wallpaper m a where
    SetWallpaper :: FilePath -> Wallpaper m ()

makeSem ''Wallpaper

runWallpaperIO :: Member (Lift IO) r => Sem (Wallpaper ': r) a -> Sem r a
runWallpaperIO = do
  --  gsettings <- Gio.settingsNew "org.gnome.desktop.background"
    undefined
