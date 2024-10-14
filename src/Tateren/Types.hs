module Tateren.Types (Tateren, Note (Note), Bgm (Bgm), BpmChange (BpmChange), Key (..), Measure (Measure), notes, bgms, bpmChanges, measures, key, value, len, start, def) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Lens.Micro.TH (makeFields, makeLenses)
import Time (HasTime (time), Time)

data Tateren = Tateren {_notes :: [Note], _bgms :: [Bgm], _bpmChanges :: [BpmChange], _measures :: IntMap Measure}

data Note = Note {_noteKey :: Key, _noteTime :: Time, _noteValue :: Int}

data Bgm = Bgm {_bgmTime :: Time, _bgmValue :: Int}

data BpmChange = BpmChange {_bpmChangeTime :: Time, _bpmChangeValue :: Int}

data Key = Sc | K1 | K2 deriving (Eq, Ord)

data Measure = Measure {len :: Float, start :: Time}

def :: Tateren
def = Tateren [] [] [] IM.empty

makeLenses ''Tateren
makeFields ''Note
makeFields ''Bgm
makeFields ''BpmChange
makeLenses ''Measure
