{-# LANGUAGE TemplateHaskell #-}

module Polysemy.FileSystem
    ( FileSystem(..)
    , writeFile
    , runFileSystemIO
    , outputToFile
    ) where

import Data.ByteString (ByteString)
import Polysemy
import Polysemy.Output

import Prelude hiding (writeFile, appendFile)
import qualified Data.ByteString as BS

data FileSystem m a where
    WriteFile :: FilePath -> ByteString -> FileSystem m ()
    AppendFile :: FilePath -> ByteString -> FileSystem m ()

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
