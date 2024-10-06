module Music (Music, frozen) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Lens.Micro.TH (makeLenses)
import qualified Music.Sounds as Sounds

data Music = Music
  { _name :: String
  , _bpm :: Float
  , _directory :: FilePath
  , _chart :: FilePath
  , _sounds :: IntMap FilePath
  }

makeLenses ''Music

frozen :: Music
frozen =
  Music
    { _name = "Frozen Bond"
    , _bpm = 106
    , _directory = "frozen"
    , _chart = "frzen.ttr"
    , _sounds = Sounds.frozen
    }