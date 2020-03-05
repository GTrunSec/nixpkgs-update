{-# LANGUAGE TemplateHaskell #-}

module Process where

import qualified Data.ByteString.Lazy as BSL
import Polysemy
import qualified System.Process.Typed as TP

data Process m a where
  ReadInterleaved :: TP.ProcessConfig stdin stdout stderr -> Process m BSL.ByteString

makeSem ''Process

runIO ::
  Member (Embed IO) r =>
  Sem (Process ': r) a ->
  Sem r a
runIO =
  interpret $ \case
    ReadInterleaved config -> embed $ TP.readProcessInterleaved_ config
