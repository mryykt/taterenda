{-# LANGUAGE OverloadedStrings #-}

module Game.Config (module Game.Config) where

import Data.Aeson.Micro

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