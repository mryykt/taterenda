module Game (mainLoop) where

import Control.Monad.Extra (notM, whenJustM, whileM)
import Control.Monad.State.Strict
  ( StateT
  , evalStateT
  , lift
  )
import qualified Game.Resource as Resource
import Game.Types (AppState (InitState, LoadState), Game (Game), Load (Load, sounds), Textures (Textures), appState, defConfig, musicList, window)
import Lens.Micro.Mtl (use, zoom, (.=))
import Music (Music (directory))
import qualified Music
import Raylib.Core (clearBackground, closeWindow, initWindow, setTargetFPS, windowShouldClose)
import Raylib.Core.Audio (initAudioDevice)
import Raylib.Util (drawing)
import Raylib.Util.Colors (white)
import System.FilePath ((</>))
import qualified Tateren
import Prelude hiding (init)

mainLoop :: IO ()
mainLoop = init >>= evalStateT (whileM (notM $ update >> draw >> shouldClose) >> teardown)

init :: IO Game
init = do
  w <- initWindow 640 480 "taterenda"
  ts <-
    Textures
      <$> Resource.loadTexture "font.bmp"
      <*> Resource.loadTexture "select.bmp"
      <*> Resource.loadTexture "skin.bmp"
      <*> Resource.loadTexture "title.bmp"
  initAudioDevice
  setTargetFPS 60
  return $ Game w defConfig Music.list ts InitState

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