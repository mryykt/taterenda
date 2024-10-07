{-# LANGUAGE OverloadedStrings #-}

module Game.Types (module Game.Types) where

import Data.Aeson.Micro
  ( FromJSON (parseJSON)
  , ToJSON (toJSON)
  , Value (Object)
  , object
  , (.:)
  , (.=)
  )
import Data.IntMap (IntMap)
import Game.Resource (Loader)
import Lens.Micro.TH (makeLenses)
import Music (MusicList)
import Raylib.Types (Sound, Texture)
import Raylib.Util (WindowResources)
import Tateren.Types (Tateren)

data Game = Game {_window :: WindowResources, _confifg :: Config, _musicList :: MusicList, _textures :: Textures, _appState :: AppState}

data Config = Config
  { width :: Int
  , height :: Int
  , fullScreen :: Bool
  }

instance FromJSON Config where
  parseJSON (Object v) = Config <$> v .: "width" <*> v .: "height" <*> v .: "fullscreen"
  parseJSON _ = fail "parse Config failed"

instance ToJSON Config where
  toJSON config = object ["width" .= config.width, "height" .= config.height, "fullscreen" .= config.fullScreen]

defConfig :: Config
defConfig =
  Config
    { width = 640
    , height = 480
    , fullScreen = False
    }

data Textures = Textures
  { font :: Texture
  , select :: Texture
  , skin :: Texture
  , title :: Texture
  }

data AppState = InitState | LoadState Load

data Load = Load {_tateren :: Tateren, sounds :: Loader IntMap Sound}

makeLenses ''Game
