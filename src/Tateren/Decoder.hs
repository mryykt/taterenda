module Tateren.Decoder (Command (..), MeasureStart (..), Unknown (..), RawChart (..), load) where

import Control.Monad (replicateM)
import Control.Monad.Extra (ifM)
import qualified Data.ByteString as BS
import Data.Either.Extra (eitherToMaybe)
import Data.Serialize.Get (Get, getInt16le, getInt32le, runGet)
import Raylib.Core (fileExists)

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
  size <- fromEnum <$> getInt32le
  commands <- replicateM (size + 1) decodeCommand
  size2 <- fromEnum <$> getInt16le
  measureStarts <- replicateM (size2 + 1) decodeMeasureStart
  unknowns <-
    replicateM
      (size2 + 1)
      decodeUnknown
  return $
    RawChart commands measureStarts unknowns

load :: FilePath -> IO (Maybe RawChart)
load path = do
  ifM
    (fileExists path)
    (eitherToMaybe . runGet getRawChart <$> BS.readFile path)
    (return Nothing)
