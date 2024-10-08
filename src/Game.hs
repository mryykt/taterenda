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
import Game.Textures (Textures (..))
import qualified Game.Textures as Textures
import Game.Types (AppState (..), Game (Game), Load (..), appState, drawer, textures, window)
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
  ts <- Textures.init
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
  (dtexture, dtext) <- use drawer
  t <- use textures
  state <- use appState
  lift $ drawing $ do
    clearBackground black
    case state of
      TitleState -> do
        dtexture t.title (Draw.vec (-60) (-80)) (Draw.rect 1 1 120 160)
        dtext "START" (Draw.vec 0 40) True
        dtext "HISCORE" (Draw.vec 0 53) True
        dtext "QUIT" (Draw.vec 0 66) True
      _ -> return ()

shouldClose :: StateT Game IO Bool
shouldClose = lift windowShouldClose

teardown :: StateT Game IO ()
teardown = do
  w <- use window
  lift $ closeWindow $ Just w