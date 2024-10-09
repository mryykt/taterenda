module Game.Types (module Game.Types) where

import Data.IntMap (IntMap)
import Game.Animation (Animation)
import Game.Config (Config)
import Game.Draw (Rectangle, Texture, Vector)
import Game.Resource (Loader)
import Lens.Micro (Lens', lens)
import Lens.Micro.TH (makeLenses)
import Music (MusicList)
import Raylib.Types (Sound)
import Raylib.Util (WindowResources)
import Tateren.Types (Tateren)

data Game = Game {_window :: WindowResources, _config :: Config, _drawer :: (Texture -> Vector -> Rectangle -> IO (), String -> Vector -> Bool -> IO ()), _musicList :: MusicList, _textures :: Textures, _appState :: AppState}

data Textures = Textures
  { font :: Texture
  , select :: Texture
  , skin :: Texture
  , title :: Texture
  }

data AppState = InitState | TitleState Title | LoadState Load

data Title = Title {_cursor :: Int, _bar :: Animation}

data Load = Load {_tateren :: Tateren, sounds :: Loader IntMap Sound}

makeLenses ''Game
makeLenses ''Title

titleState :: Lens' AppState Title
titleState = lens getter setter
  where
    getter (TitleState x) = x
    getter _ = error "error"
    setter (TitleState _) y = TitleState y
    setter _ _ = undefined