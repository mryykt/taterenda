module Tateren.Types(Tateren,notes,bgms,bpmChanges,measures,key,time,value,len,start) where

import Data.IntMap(IntMap)
import Lens.Micro.TH (makeLenses, makeFields)

data Tateren=Tateren{_notes::[Note],_bgms::[Bgm],_bpmChanges::[BpmChange],_measures::IntMap Measure}

data Note=Note{_noteKey::Key,_noteTime::Float,_noteValue::Int}

data Bgm=Bgm{_bgmTime::Float,_bgmValue::Int}

data BpmChange=BpmChange{_bpmChangeTime::Float,_bpmChangeValue::Int}

data Key=Sc|K1|K2

data Measure=Measure{len::Float,start::Float}

makeLenses ''Tateren
makeFields ''Note
makeFields ''Bgm
makeFields ''BpmChange
makeLenses ''Measure