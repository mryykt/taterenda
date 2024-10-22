module Tateren.Types (Tateren, Object (Object), Note, Bgm, BpmChange, Stop, Key (..), Measure (Measure), notes, bgms, bpmChanges, stops, measures, ext, value, len, def) where

import Lens.Micro.TH (makeFields, makeLenses)
import Time (HasTime (time), Time)

data Tateren = Tateren {_notes :: [Note], _bgms :: [Bgm], _bpmChanges :: [BpmChange], _stops :: [Stop], _measures :: [Measure]}

data Object a = Object {_objectTime :: Time, _objectValue :: Int, _objectExt :: a}

type Note = Object Key
type Bgm = Object ()
type BpmChange = Object ()
type Stop = Object ()

data Key = Sc | K1 | K2 deriving (Eq, Ord)

data Measure = Measure {len :: Float, _measureTime :: Time}

def :: Tateren
def = Tateren [] [] [] [] []

makeLenses ''Tateren
makeFields ''Object
makeFields ''Measure
