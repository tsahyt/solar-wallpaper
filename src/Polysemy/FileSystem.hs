{-# LANGUAGE TemplateHaskell #-}

module Polysemy.FileSystem
    ( FileSystem(..)
    , writeFile
    , appendFile
    , readFile
    , doesFileExist
    , runFileSystemIO
    , outputToFile
    ) where

import Data.ByteString (ByteString)
import Polysemy
import Polysemy.Output

import Prelude hiding (writeFile, appendFile, readFile)
import qualified Data.ByteString as BS
import qualified System.Directory as D

data FileSystem m a where
    WriteFile :: FilePath -> ByteString -> FileSystem m ()
    AppendFile :: FilePath -> ByteString -> FileSystem m ()
    ReadFile :: FilePath -> FileSystem m ByteString
    DoesFileExist :: FilePath -> FileSystem m Bool

makeSem ''FileSystem

outputToFile ::
       Member FileSystem r
    => FilePath
    -> (a -> ByteString)
    -> Sem (Output a ': r) x
    -> Sem r x
outputToFile path convert sem = do
    writeFile path mempty
    (interpret $ \case
         Output o -> appendFile path (convert o))
        sem

runFileSystemIO :: Member (Lift IO) r => Sem (FileSystem ': r) a -> Sem r a
runFileSystemIO =
    interpret $ \case
        WriteFile path dat -> sendM $ BS.writeFile path dat
        AppendFile path dat -> sendM $ BS.appendFile path dat
        ReadFile path -> sendM $ BS.readFile path
        DoesFileExist path -> sendM $ D.doesFileExist path
