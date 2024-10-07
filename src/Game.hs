module Game (mainLoop) where

import Control.Monad.Extra (notM, whenJustM, whileM)
import Control.Monad.State.Strict
  ( StateT
  , evalStateT
  , lift
  )
import qualified Game.Resource as Resource
import Game.Types (AppState (InitState, LoadState), Game (Game), Load (Load, sounds), appState, musicList, window)
import Lens.Micro.Mtl (use, zoom, (.=))
import Music (Music (directory))
import qualified Music
import Raylib.Core (closeWindow, initWindow, setTargetFPS, windowShouldClose)
import Raylib.Core.Audio (initAudioDevice)
import System.FilePath ((</>))
import qualified Tateren
import Prelude hiding (init)

mainLoop :: IO ()
mainLoop = init >>= evalStateT (whileM (notM $ update >> draw >> shouldClose) >> teardown)

init :: IO Game
init = do
  w <- initWindow 640 480 "taterenda"
  initAudioDevice
  setTargetFPS 60
  return $ Game w Music.list InitState

update :: StateT Game IO ()
update = do
  aps <- use appState
  case aps of
    InitState -> do
      curr <- zoom musicList Music.current
      let dir = "sound" </> curr.directory
      tate <- lift $ Tateren.load $ dir </> curr.chart
      loader <- lift $ Resource.soundLoader $ (dir </>) <$> curr.sounds
      appState .= LoadState (Load tate loader)
    LoadState ld -> do
      whenJustM (lift $ Resource.get ld.sounds) (\_ -> return ())
  return ()

draw :: StateT Game IO ()
draw = return ()

shouldClose :: StateT Game IO Bool
shouldClose = lift windowShouldClose

teardown :: StateT Game IO ()
teardown = do
  w <- use window
  lift $ closeWindow $ Just w