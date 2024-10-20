module Tateren.Types (Tateren, Note (Note), Bgm (Bgm), BpmChange (BpmChange), Stop (Stop), Key (..), Measure (Measure), notes, bgms, bpmChanges, stops, measures, key, value, len, def) where

import Lens.Micro.TH (makeFields, makeLenses)
import Time (HasTime (time), Time)

data Tateren = Tateren {_notes :: [Note], _bgms :: [Bgm], _bpmChanges :: [BpmChange], _stops :: [Stop], _measures :: [Measure]}

data Note = Note {_noteKey :: Key, _noteTime :: Time, _noteValue :: Int}

data Bgm = Bgm {_bgmTime :: Time, _bgmValue :: Int}

data BpmChange = BpmChange {_bpmChangeTime :: Time, _bpmChangeValue :: Int}

data Stop = Stop {_stopTime :: Time, _stopValue :: Int}

data Key = Sc | K1 | K2 deriving (Eq, Ord)

data Measure = Measure {len :: Float, _measureTime :: Time}

def :: Tateren
def = Tateren [] [] [] [] []

makeLenses ''Tateren
makeFields ''Note
makeFields ''Bgm
makeFields ''BpmChange
makeFields ''Stop
makeFields ''Measure
