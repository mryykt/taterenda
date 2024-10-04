module Tateren.Decoder(Command(..),MeasureStart(..),Unknown(..),RawChart(..),decodeChart) where

import Codec.Serialise.Decoding
import Control.Monad (replicateM)

data Command=Command Int Int Int Int Int

data MeasureStart=MeasureStart Int Int

data Unknown=Unknown Int Int Int Int Int

data RawChart=RawChart [Command] [MeasureStart] [Unknown]

decodeChart::Decoder s RawChart
decodeChart=do
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