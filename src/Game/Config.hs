{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Game.Config (Config (..), EditMode, initEditMode, read, write, update, apply, setWindow) where

import Control.Monad.Extra (ifM, unlessM, whenM)
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
import GHC.Float (int2Float)
import Raylib.Core
  ( clearBackground
  , fileExists
  , getKeyPressed
  , getScreenHeight
  , getScreenWidth
  , isWindowState
  , setWindowSize
  , toggleBorderlessWindowed
  )
import Raylib.Types
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
          let
            fontSize = min 30 $ max 15 $ min (cfg.actualWidth / 100.0) (cfg.actualHeight / 50.0)
            elementHeight = fontSize + 5
            labelWidth = 10 + 10 * (fontSize + 1)
            formWidth = min (fontSize * 20) $ cfg.actualWidth - labelWidth - 20
          guiSetStyleTextSize (round fontSize)
          drawing $ do
            clearBackground Colors.black
            guiGroupBox (Rectangle 10 10 (cfg.actualWidth - 20) (5.5 * elementHeight)) (Just "window")
            guiLabel (Rectangle 20 (10 + elementHeight) labelWidth elementHeight) "width"
            (wem, width') <- guiValueBox (Rectangle labelWidth (10 + elementHeight) formWidth elementHeight) Nothing cfg.width 0 10000 (editMode == Width)
            guiLabel (Rectangle 20 (10 + 2.5 * elementHeight) labelWidth elementHeight) "height"
            (hem, height') <- guiValueBox (Rectangle labelWidth (10 + 2.5 * elementHeight) formWidth elementHeight) Nothing cfg.height 0 10000 (editMode == Height)
            guiLabel (Rectangle 20 (10 + 4.0 * elementHeight) labelWidth elementHeight) "fullscreen"
            fullScreen' <- guiCheckBox (Rectangle labelWidth (10 + 4.0 * elementHeight) 20 elementHeight) Nothing cfg.fullScreen

            guiGroupBox (Rectangle 10 (10 + 6 * elementHeight) (cfg.actualWidth - 20) (5.5 * elementHeight)) (Just "key config")
            guiLabel (Rectangle 20 (10 + 6.5 * elementHeight) labelWidth elementHeight) "scratch key"
            (scem, scratchKey') <- keyConfig (Rectangle labelWidth (10 + 6.5 * elementHeight) formWidth elementHeight) cfg.scratchKey (editMode == ScKey)
            guiLabel (Rectangle 20 (10 + 8 * elementHeight) labelWidth elementHeight) "1-key"
            (k1em, key1') <- keyConfig (Rectangle labelWidth (10 + 8 * elementHeight) formWidth elementHeight) cfg.key1 (editMode == Key1)
            guiLabel (Rectangle 20 (10 + 9.5 * elementHeight) labelWidth elementHeight) "2-key"
            (k2em, key2') <- keyConfig (Rectangle labelWidth (10 + 9.5 * elementHeight) formWidth elementHeight) cfg.key2 (editMode == Key2)

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

apply :: StateT Config IO ()
apply = do
  cfg <- get
  cfg' <- lift $ do
    write cfg
    setWindow cfg
  put cfg'

setWindow :: Config -> IO Config
setWindow cfg = do
  if cfg.fullScreen
    then unlessM isWindowBorderless toggleBorderlessWindowed
    else do
      whenM isWindowBorderless toggleBorderlessWindowed
      setWindowSize cfg.width cfg.height
  aw <- getScreenWidth
  ah <- getScreenHeight
  return cfg{actualWidth = int2Float aw, actualHeight = int2Float ah}

isWindowBorderless :: IO Bool
isWindowBorderless = isWindowState [BorderlessWindowedMode]

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