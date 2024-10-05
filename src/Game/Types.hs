module Game.Types (module Game.Types) where

import Lens.Micro.TH (makeLenses)
import Raylib.Util (WindowResources)

data Game = Game {_window :: WindowResources, _appState :: AppState}

data AppState = AppState

makeLenses ''Game
