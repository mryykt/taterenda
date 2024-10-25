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
  ( Key (..)
  , Measure (Measure)
  , Object (Object)
  , Tateren
  , bgms
  , bpmChanges
  , def
  , measures
  , notes
  , stops
  )
import qualified Time

fromRawData :: Bool -> RawChart -> Tateren
fromRawData mirror (RawChart cs mss _) = def & measures .~ ms & flip (foldr convertCommands) filtered
  where
    filtered = filter (\(Command _ _ v _ _) -> v /= 0) cs
    ms = (\(MeasureStart l s) -> Measure (int2Float l) (Time.fromInt s)) <$> mss
    convertCommands (Command typ t v _ _) = case typ of
      0 -> bgms %~ (Object (Time.fromInt t) v () :)
      1 -> bpmChanges %~ (Object (Time.fromInt t) v () :)
      2 -> stops %~ (Object (Time.fromInt t) v () :)
      0xb -> notes %~ (Object (Time.fromInt t) v (if mirror then K2 else K1) :)
      0xc -> notes %~ (Object (Time.fromInt t) v (if mirror then K1 else K2) :)
      0xd -> notes %~ (Object (Time.fromInt t) v Sc :)
      _ -> error ("ここに来たとしたら実装漏れ:" ++ show typ)

load :: Bool -> FilePath -> IO (Maybe Tateren)
load mirror = fmap (fmap (fromRawData mirror)) . D.load
