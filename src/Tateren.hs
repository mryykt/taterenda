module Tateren(fromRawData,load) where

import Tateren.Types
    ( Tateren,
      Measure(Measure),
      Bgm(Bgm),
      BpmChange(BpmChange),
      Note(Note),
      Key(Sc, K1),
      def,
      measures,
      bgms,
      bpmChanges,
      notes )
import qualified Data.IntMap as IM
import GHC.Float (int2Float)
import Lens.Micro ( (&), (.~), (%~) )
import Tateren.Decoder
    ( RawChart(..), Command(Command), MeasureStart(MeasureStart) )
import qualified Tateren.Decoder as D

fromRawData::RawChart->Tateren
fromRawData (RawChart cs mss _)=def&measures .~ms &flip (foldr convertCommands) cs
  where
    ms=IM.fromList $ zip [1..] $ (\(MeasureStart l s)->Measure (int2Float l) (int2Float s)) <$> mss
    convertCommands (Command typ t v _ _) =case typ of
          0->bgms%~(Bgm (int2Float t) v:)
          1->bpmChanges%~(BpmChange (int2Float t) v:)
          0xb->notes%~(Note K1 (int2Float t) v:)
          0xc->notes%~(Note K1 (int2Float t) v:)
          0xd->notes%~(Note Sc (int2Float t) v:)
          _->error "ここに来たとしたら実装漏れ"

load::FilePath->IO Tateren
load =(fromRawData <$>) . D.load 