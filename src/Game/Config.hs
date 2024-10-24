{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Game.Config (Config (..), EditMode, initEditMode, read, write, update) where

import Control.Monad.Extra (ifM)
import Control.Monad.State.Strict (MonadState (get), MonadTrans (lift), StateT, put)
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
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Raylib.Core (clearBackground, fileExists, getKeyPressed)
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

data EditMode = Width | Height | ScKey | Key1 | Key2 | NoFocus deriving (Eq)

initEditMode :: EditMode
initEditMode = Width

write :: Config -> IO ()
write = BS.writeFile "config.json" . encodeStrict

read :: IO Config
read =
  ifM
    (fileExists "config.json")
    (fromMaybe (error "config file is invalid") . decodeStrict <$> BS.readFile "config.json")
    (write def >> return def)

update :: EditMode -> StateT Config IO EditMode
update editMode = do
  cfg <- get
  (cfg', editMode') <-
    lift
      ( do
          cfg' <- newIORef cfg
          editMode' <- newIORef editMode
          drawing $ do
            clearBackground Colors.black
            guiGroupBox (Rectangle 10 10 500 200) (Just "window")
            (wem, width') <- guiValueBox (Rectangle 100 20 400 20) (Just "width") cfg.width 0 10000 (editMode == Width)
            (hem, height') <- guiValueBox (Rectangle 100 45 400 20) (Just "height") cfg.height 0 10000 (editMode == Height)
            fullScreen' <- guiCheckBox (Rectangle 100 70 20 20) (Just "fullscreen") cfg.fullScreen

            guiGroupBox (Rectangle 10 220 500 200) (Just "key config")
            (scem, scratchKey') <- keyConfig (Rectangle 100 230 400 20) cfg.scratchKey (editMode == ScKey)
            (k1em, key1') <- keyConfig (Rectangle 100 255 400 20) cfg.key1 (editMode == Key1)
            (k2em, key2') <- keyConfig (Rectangle 100 280 400 20) cfg.key2 (editMode == Key2)

            writeIORef cfg' cfg{width = width', height = height', fullScreen = fullScreen', scratchKey = scratchKey', key1 = key1', key2 = key2'}
            writeIORef
              editMode'
              $ if
                | wem && editMode /= Width -> Width
                | hem && editMode /= Height -> Height
                | cfg.fullScreen /= fullScreen' -> NoFocus
                | scem && editMode /= ScKey -> ScKey
                | scem -> NoFocus
                | k1em && editMode /= Key1 -> Key1
                | k1em -> NoFocus
                | k2em && editMode /= Key2 -> Key2
                | k2em -> NoFocus
                | otherwise -> editMode
            return ()
          (,) <$> readIORef cfg' <*> readIORef editMode'
      )
  put cfg'
  return editMode'

keyConfig :: Rectangle -> KeyboardKey -> Bool -> IO (Bool, KeyboardKey)
keyConfig rect value editMode = do
  (toggled, _) <- guiTextBox rect (show value) Nothing editMode
  if editMode
    then do
      k <- getKeyPressed
      if k /= KeyNull
        then return (True, k)
        else return (False, value)
    else
      return (toggled, value)

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