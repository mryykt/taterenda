module Game (mainLoop) where

import Control.Monad.Extra (notM, when, whenJustM, whileM)
import Control.Monad.State.Strict
  ( StateT
  , evalStateT
  , lift
  )
import Data.Aeson.Micro (decodeStrict, encodeStrict)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import GHC.Float (int2Float)
import Game.Config (Config (..), defConfig)
import qualified Game.Draw as Draw
import qualified Game.Resource as Resource
import Game.Types (AppState (..), Game (Game), Load (..), Textures (..), appState, drawer, textures, window)
import Lens.Micro.Mtl (use, (.=))
import qualified Music
import Raylib.Core (clearBackground, closeWindow, fileExists, getScreenHeight, getScreenWidth, initWindow, setTargetFPS, setTraceLogLevel, toggleBorderlessWindowed, windowShouldClose)
import Raylib.Core.Audio (initAudioDevice)
import Raylib.Types (TraceLogLevel (LogNone))
import Raylib.Util (drawing)
import Raylib.Util.Colors (black)
import Prelude hiding (init)

mainLoop :: IO ()
mainLoop = init >>= evalStateT (whileM (notM $ update >> draw >> shouldClose) >> teardown)

init :: IO Game
init = do
  isConfigExists <- fileExists "config.json"
  config <-
    if isConfigExists
      then fromMaybe (error "config file is invalid") . decodeStrict <$> BS.readFile "config.json"
      else BS.writeFile "config.json" (encodeStrict defConfig) >> return defConfig
  w <- initWindow config.width config.height "taterenda"
  when config.fullScreen toggleBorderlessWindowed
  ts <-
    Textures
      <$> Resource.loadTexture "font.bmp"
      <*> Resource.loadTexture "select.bmp"
      <*> Resource.loadTexture "skin.bmp"
      <*> Resource.loadTexture "title.bmp"
  aw <- getScreenWidth
  ah <- getScreenHeight
  let config' = config{actualWidth = int2Float aw, actualHeight = int2Float ah}
  initAudioDevice
  setTargetFPS 60
  setTraceLogLevel LogNone
  let drawTexture = Draw.texture config'
  return $ Game w config' (drawTexture, Draw.text drawTexture ts.font) Music.list ts InitState

update :: StateT Game IO ()
update = do
  state <- use appState
  case state of
    InitState -> do
      curr <- zoom musicList Music.current
      let dir = "sound" </> curr.directory
      tate <- lift $ Tateren.load $ dir </> curr.chart
      loader <- lift $ Resource.soundLoader $ (dir </>) <$> curr.sounds
      appState .= LoadState (Load tate loader)
    LoadState ld -> do
      whenJustM (lift $ Resource.get ld.sounds) (\_ -> return ())

draw :: StateT Game IO ()
draw = lift $ drawing $ clearBackground white

shouldClose :: StateT Game IO Bool
shouldClose = lift windowShouldClose

teardown :: StateT Game IO ()
teardown = do
  w <- use window
  lift $ closeWindow $ Just w