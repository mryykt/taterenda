module Tateren.Decoder (Command (..), MeasureStart (..), Unknown (..), RawChart (..), load) where

import Control.Monad (replicateM)
import qualified Data.ByteString as BS
import Data.Serialize.Get

data Command = Command Int Int Int Int Int

data MeasureStart = MeasureStart Int Int

data Unknown = Unknown Int Int Int Int Int

data RawChart = RawChart [Command] [MeasureStart] [Unknown]

getRawChart :: Get RawChart
getRawChart = do
  let
    getInt = fromEnum <$> getInt32le
    decodeCommand = Command <$> getInt <*> getInt <*> getInt <*> getInt <*> getInt
    decodeMeasureStart = MeasureStart <$> getInt <*> getInt
    decodeUnknown = Unknown <$> getInt <*> getInt <*> getInt <*> getInt <*> getInt
  size <- fromEnum <$> getInt16le
  commands <- replicateM (size + 1) decodeCommand
  size2 <- fromEnum <$> getInt16le
  measureStarts <- replicateM (size2 + 1) decodeMeasureStart
  unknowns <-
    replicateM
      (size2 + 1)
      decodeUnknown
  return $
    RawChart commands measureStarts unknowns

load :: FilePath -> IO RawChart
load = fmap (either undefined id . runGet getRawChart) . BS.readFile
