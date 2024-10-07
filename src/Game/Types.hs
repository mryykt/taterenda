module Game.Types (module Game.Types) where

import Data.IntMap (IntMap)
import Game.Resource (Loader)
import Lens.Micro.TH (makeLenses)
import Music (MusicList)
import Raylib.Types (Sound, Texture)
import Raylib.Util (WindowResources)
import Tateren.Types (Tateren)

data Game = Game {_window :: WindowResources, _musicList :: MusicList, _textures :: Textures, _appState :: AppState}

data Textures = Textures
  { font :: Texture
  , select :: Texture
  , skin :: Texture
  , title :: Texture
  }

data AppState = InitState | LoadState Load

data Load = Load {_tateren :: Tateren, sounds :: Loader IntMap Sound}

makeLenses ''Game
