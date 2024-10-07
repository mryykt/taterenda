module Game.Types (module Game.Types) where

import Data.IntMap (IntMap)
import Game.Resource (Loader)
import Lens.Micro.TH (makeLenses)
import Music (MusicList)
import Raylib.Types (Sound)
import Raylib.Util (WindowResources)
import Tateren.Types (Tateren)

data Game = Game {_window :: WindowResources, _musicList :: MusicList, _appState :: AppState}

data AppState = InitState | LoadState Load

data Load = Load {_tateren :: Tateren, sounds :: Loader IntMap Sound}

makeLenses ''Game
