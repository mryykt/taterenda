module Tateren.Decoder(Command(..),MeasureStart(..),Unknown(..),RawChart(..),load) where

import Codec.Serialise.Decoding
import Control.Monad (replicateM)
import Codec.Serialise (Serialise(..), readFileDeserialise)

data Command=Command Int Int Int Int Int

data MeasureStart=MeasureStart Int Int

data Unknown=Unknown Int Int Int Int Int

data RawChart=RawChart [Command] [MeasureStart] [Unknown]

instance Serialise RawChart where
  encode=undefined
  decode=do
    let
      decodeCommand=Command<$>decodeInt<*>decodeInt<*>decodeInt<*>decodeInt<*>decodeInt
      decodeMeasureStart=MeasureStart<$>decodeInt<*>decodeInt
      decodeUnknown=Unknown<$>decodeInt<*>decodeInt<*>decodeInt<*>decodeInt<*>decodeInt
    size<-decodeInt
    commands<-replicateM (size+1)  decodeCommand
    size2<-fromEnum<$>decodeInt16
    measureStarts<-replicateM (size2+1) decodeMeasureStart
    unknowns<-replicateM (size2+1) decodeUnknown
    return $ RawChart commands measureStarts unknowns

load::FilePath->IO RawChart
load=readFileDeserialise