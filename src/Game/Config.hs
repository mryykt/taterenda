{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Game.Config (Config (..), read, write, update) where

import Control.Monad.Extra (ifM)
import Control.Monad.State.Strict (MonadTrans (lift), StateT)
import Data.Aeson.Micro
  ( FromJSON (..)
  , ToJSON (..)
  , Value (Object)
  , decodeStrict
  , encodeStrict
  , object
  , (.:)
  , (.=)
  )
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Raylib.Core (clearBackground, fileExists)
import Raylib.Types (KeyboardKey (..), Rectangle (Rectangle))
import Raylib.Util (drawing)
import qualified Raylib.Util.Colors as Colors
import Raylib.Util.GUI
import Prelude hiding (read)
import qualified Prelude

data Config = Config
  { width :: Int
  , height :: Int
  , fullScreen :: Bool
  , scratchKey :: KeyboardKey
  , key1 :: KeyboardKey
  , key2 :: KeyboardKey
  , actualWidth :: Float
  , actualHeight :: Float
  }

deriving instance Read KeyboardKey

instance FromJSON Config where
  parseJSON (Object v) =
    Config
      <$> v .: "width"
      <*> v .: "height"
      <*> v .: "fullscreen"
      <*> fmap (Prelude.read . Text.unpack) (v .: "scratch_key")
      <*> fmap (Prelude.read . Text.unpack) (v .: "key1")
      <*> fmap (Prelude.read . Text.unpack) (v .: "key2")
      <*> return 0
      <*> return 0
  parseJSON _ = fail "parse Config failed"

instance ToJSON Config where
  toJSON config =
    object
      [ "width" .= config.width
      , "height" .= config.height
      , "fullscreen" .= config.fullScreen
      , "scratch_key" .= Text.pack (show config.scratchKey)
      , "key1" .= Text.pack (show config.key1)
      , "key2" .= Text.pack (show config.key2)
      ]

write :: Config -> IO ()
write = BS.writeFile "config.json" . encodeStrict

read :: IO Config
read =
  ifM
    (fileExists "config.json")
    (fromMaybe (error "config file is invalid") . decodeStrict <$> BS.readFile "config.json")
    (write def >> return def)

update :: StateT Config IO ()
update = do
  lift $ drawing $ do
    clearBackground Colors.black
    guiGroupBox (Rectangle 10 10 500 200) (Just "window")

def :: Config
def =
  Config
    { width = 640
    , height = 480
    , fullScreen = False
    , scratchKey = KeyLeftShift
    , key1 = KeyZ
    , key2 = KeyX
    , actualWidth = 0
    , actualHeight = 0
    }