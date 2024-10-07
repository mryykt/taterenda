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
import Game.Types (AppState (..), Game (Game), Load (..), Textures (..), appState, config, renderTexture, textures, window)
import Lens.Micro.Mtl (use, (.=))
import qualified Music
import Raylib.Core (closeWindow, fileExists, getScreenHeight, getScreenWidth, initWindow, setTargetFPS, setTraceLogLevel, toggleBorderlessWindowed, windowShouldClose)
import Raylib.Core.Audio (initAudioDevice)
import Raylib.Core.Textures (loadRenderTexture)
import Raylib.Types (TraceLogLevel (LogNone))
import Prelude hiding (init)

mainLoop :: IO ()
mainLoop = init >>= evalStateT (whileM (notM $ update >> draw >> shouldClose) >> teardown)

init :: IO Game
init = do
  isConfigExists <- fileExists "config.json"
  cfg <-
    if isConfigExists
      then fromMaybe (error "config file is invalid") . decodeStrict <$> BS.readFile "config.json"
      else BS.writeFile "config.json" (encodeStrict defConfig) >> return defConfig
  w <- initWindow cfg.width cfg.height "taterenda"
  when cfg.fullScreen toggleBorderlessWindowed
  ts <-
    Textures
      <$> Resource.loadTexture "font.bmp"
      <*> Resource.loadTexture "select.bmp"
      <*> Resource.loadTexture "skin.bmp"
      <*> Resource.loadTexture "title.bmp"
  aw <- getScreenWidth
  ah <- getScreenHeight
  let cfg' = cfg{actualWidth = int2Float aw, actualHeight = int2Float ah}
  initAudioDevice
  setTargetFPS 60
  setTraceLogLevel LogNone
  rt <- loadRenderTexture 120 160
  return $ Game w cfg' rt Music.list ts InitState

update :: StateT Game IO ()
update = do
  state <- use appState
  case state of
    InitState -> appState .= TitleState
    TitleState -> return ()
    LoadState ld -> do
      whenJustM (lift $ Resource.get ld.sounds) (\_ -> return ())

{-
curr <- zoom musicList Music.current
let dir = "sound" </> curr.directory
tate <- lift $ Tateren.load $ dir </> curr.chart
loader <- lift $ Resource.soundLoader $ (dir </>) <$> curr.sounds
appState .= LoadState (Load tate loader)
-}

draw :: StateT Game IO ()
draw = do
  t <- use textures
  rt <- use renderTexture
  cfg <- use config
  let dtext = Draw.text Draw.texture t.font
  state <- use appState
  lift $ Draw.drawing cfg rt $ do
    case state of
      TitleState -> do
        Draw.texture t.title (Draw.vec 0 0) (Draw.rect 1 1 120 160)
        Draw.texture t.font (Draw.vec 20 30) (Draw.rect 199 31 80 9)
        dtext "START" (Draw.vec 60 30) True
        dtext "HISCORE" (Draw.vec 60 17) True
        dtext "QUIT" (Draw.vec 60 4) True
      _ -> return ()

shouldClose :: StateT Game IO Bool
shouldClose = lift windowShouldClose

teardown :: StateT Game IO ()
teardown = do
  w <- use window
  lift $ closeWindow $ Just w