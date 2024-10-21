module Game.Config (Config (..), read, write) where

import Control.Monad.Extra (ifM)
import Data.Aeson.Micro
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Raylib.Core (fileExists)
import Prelude hiding (read)

data Config = Config
  { width :: Int
  , height :: Int
  , fullScreen :: Bool
  , actualWidth :: Float
  , actualHeight :: Float
  }

instance FromJSON Config where
  parseJSON (Object v) = Config <$> v .: "width" <*> v .: "height" <*> v .: "fullscreen" <*> return 0 <*> return 0
  parseJSON _ = fail "parse Config failed"

instance ToJSON Config where
  toJSON config = object ["width" .= config.width, "height" .= config.height, "fullscreen" .= config.fullScreen]

write :: Config -> IO ()
write = BS.writeFile "config.json" . encodeStrict

read :: IO Config
read =
  ifM
    (fileExists "config.json")
    (fromMaybe (error "config file is invalid") . decodeStrict <$> BS.readFile "config.json")
    (write def >> return def)

def :: Config
def =
  Config
    { width = 640
    , height = 480
    , fullScreen = False
    , actualWidth = 0
    , actualHeight = 0
    }