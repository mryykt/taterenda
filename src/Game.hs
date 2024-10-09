module Game (mainLoop) where

import Control.Monad.Extra (notM, orM, when, whenJustM, whenM, whileM)
import Control.Monad.State.Strict
  ( StateT
  , evalStateT
  , lift
  )
import Data.Aeson.Micro (decodeStrict, encodeStrict)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import GHC.Float (int2Float)
import qualified Game.Animation as Animation
import Game.Config (Config (..), defConfig)
import Game.Draw ((|+|), (|-|))
import qualified Game.Draw as Draw
import qualified Game.Resource as Resource
import Game.Types (AppState (..), Game (Game), Load (..), Textures (..), Title (Title), appState, bar, cursor, drawer, textures, titleState, window)
import Lens.Micro (Lens', (^.))
import Lens.Micro.Mtl (use, zoom, (%=), (+=), (-=), (.=))
import qualified Music
import Raylib.Core (clearBackground, closeWindow, fileExists, getFrameTime, getScreenHeight, getScreenWidth, initWindow, isKeyPressed, setTargetFPS, setTraceLogLevel, toggleBorderlessWindowed, windowShouldClose)
import Raylib.Core.Audio (initAudioDevice)
import Raylib.Types (KeyboardKey (KeyDown, KeyLeft, KeyRight, KeyUp), TraceLogLevel (LogNone))
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
  dt <- lift getFrameTime
  case state of
    InitState -> appState .= TitleState (Title 0 (Animation.init (Draw.vec (-40) 40)))
    TitleState tit ->
      zoom (appState . titleState) $ do
        whenM (((tit ^. cursor > 0) &&) <$> orM [lift $ isKeyPressed KeyUp, lift $ isKeyPressed KeyLeft]) $ do
          b <- bar %?= Animation.to (Animation.get (tit ^. bar) |-| Draw.vec 0 13) (Draw.vec 0 (-50))
          cursor -= fromEnum b
        whenM (((tit ^. cursor < 2) &&) <$> orM [lift $ isKeyPressed KeyDown, lift $ isKeyPressed KeyRight]) $ do
          b <- bar %?= Animation.to (Animation.get (tit ^. bar) |+| Draw.vec 0 13) (Draw.vec 0 50)
          cursor += fromEnum b
        bar %= Animation.update dt
    LoadState ld -> do
      whenJustM (lift $ Resource.get ld.sounds) (\_ -> return ())

(%?=) :: Lens' a b -> (b -> Maybe b) -> StateT a IO Bool
l %?= f = do
  x <- use l
  let y = f x
  case y of
    Just y' -> l .= y' >> return True
    Nothing -> return False

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
      TitleState tit -> do
        dtexture t.title (Draw.vec (-60) (-80)) (Draw.rect 1 1 120 160)
        dtexture t.font (Animation.get $ tit ^. bar) (Draw.rect 199 31 80 9)
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