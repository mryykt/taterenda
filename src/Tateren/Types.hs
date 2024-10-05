module Tateren.Types (Tateren, Note (Note), Bgm (Bgm), BpmChange (BpmChange), Key (..), Measure (Measure), notes, bgms, bpmChanges, measures, key, time, value, len, start, def) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Lens.Micro.TH (makeFields, makeLenses)

data Tateren = Tateren {_notes :: [Note], _bgms :: [Bgm], _bpmChanges :: [BpmChange], _measures :: IntMap Measure}

data Note = Note {_noteKey :: Key, _noteTime :: Float, _noteValue :: Int}

data Bgm = Bgm {_bgmTime :: Float, _bgmValue :: Int}

data BpmChange = BpmChange {_bpmChangeTime :: Float, _bpmChangeValue :: Int}

data Key = Sc | K1 | K2

data Measure = Measure {len :: Float, start :: Float}

def :: Tateren
def = Tateren [] [] [] IM.empty

makeLenses ''Tateren
makeFields ''Note
makeFields ''Bgm
makeFields ''BpmChange
makeLenses ''Measure
