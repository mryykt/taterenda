module Tateren (load) where

import GHC.Float (int2Float)
import Lens.Micro ((%~), (&), (.~))
import Tateren.Decoder
  ( Command (Command)
  , MeasureStart (MeasureStart)
  , RawChart (..)
  )
import qualified Tateren.Decoder as D
import Tateren.Types
  ( Bgm (Bgm)
  , BpmChange (BpmChange)
  , Key (..)
  , Measure (Measure)
  , Note (Note)
  , Stop (Stop)
  , Tateren
  , bgms
  , bpmChanges
  , def
  , measures
  , notes
  , stops
  )
import qualified Time

fromRawData :: RawChart -> Tateren
fromRawData (RawChart cs mss _) = def & measures .~ ms & flip (foldr convertCommands) cs
  where
    ms = (\(MeasureStart l s) -> Measure (int2Float l) (Time.fromInt s)) <$> mss
    convertCommands (Command typ t v _ _) = case typ of
      0 -> bgms %~ (Bgm (Time.fromInt t) v :)
      1 -> bpmChanges %~ (BpmChange (Time.fromInt t) v :)
      2 -> stops %~ (Stop (Time.fromInt t) v :)
      0xb -> notes %~ (Note K1 (Time.fromInt t) v :)
      0xc -> notes %~ (Note K2 (Time.fromInt t) v :)
      0xd -> notes %~ (Note Sc (Time.fromInt t) v :)
      _ -> error ("ここに来たとしたら実装漏れ:" ++ show typ)

load :: FilePath -> IO Tateren
load = (fromRawData <$>) . D.load
