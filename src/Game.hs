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
import Game.Types (AppState (..), Game (Game), Load (..), Select (Select), Textures (..), Title (Title), TitleCusor (..), appState, bar, close, cursor, drawer, musicList, textures, titleState, window)
import Lens.Micro (Lens', (^.))
import Lens.Micro.Mtl (use, zoom, (%=), (.=))
import qualified Music
import Raylib.Core (clearBackground, closeWindow, fileExists, getFrameTime, getScreenHeight, getScreenWidth, initWindow, isKeyPressed, setTargetFPS, setTraceLogLevel, toggleBorderlessWindowed)
import Raylib.Core.Audio (initAudioDevice)
import Raylib.Types (KeyboardKey (KeyDown, KeyEnter, KeyEscape, KeyLeft, KeyRight, KeyUp), TraceLogLevel (LogNone))
import Raylib.Util (drawing)
import Raylib.Util.Colors (black)
import Text.Printf (printf)
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
  return $ Game w config' False (drawTexture, Draw.text drawTexture ts.font) Music.list ts InitState

update :: StateT Game IO ()
update = do
  state <- use appState
  dt <- lift getFrameTime
  case state of
    InitState -> appState .= initTitle
    TitleState tit -> do
      zoom (appState . titleState) $
        do
          whenM (((tit ^. cursor > Start) &&) <$> orM [lift $ isKeyPressed KeyUp, lift $ isKeyPressed KeyLeft]) $ do
            b <- bar %?= Animation.to (Animation.get (tit ^. bar) |-| Draw.vec 0 13) (Draw.vec 0 (-50))
            when b $ cursor %= pred
          whenM (((tit ^. cursor < Quit) &&) <$> orM [lift $ isKeyPressed KeyDown, lift $ isKeyPressed KeyRight]) $ do
            b <- bar %?= Animation.to (Animation.get (tit ^. bar) |+| Draw.vec 0 13) (Draw.vec 0 50)
            when b $ cursor %= succ
          bar %= Animation.update dt
      whenM
        (lift $ isKeyPressed KeyEnter)
        $ case tit ^. cursor of
          Start -> appState .= SelectState (Select (Animation.init (Draw.vec 0 0)) (Animation.init (Draw.vec 0 0)))
          HiScore -> undefined
          Quit -> close .= True
      whenM (lift $ isKeyPressed KeyEscape) (close .= True)
    SelectState _ -> do
      whenM
        (lift $ isKeyPressed KeyRight)
        $ musicList %= Music.next
      whenM
        (lift $ isKeyPressed KeyLeft)
        $ musicList %= Music.prev
      whenM
        (lift $ isKeyPressed KeyEscape)
        (appState .= initTitle)
    LoadState ld -> do
      whenJustM (lift $ Resource.get ld.sounds) (\_ -> return ())

initTitle :: AppState
initTitle = TitleState (Title Start (Animation.init (Draw.vec (-40) 40)))

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
  ml <- use musicList
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
      SelectState _ -> do
        let (index, music) = Music.current ml
        dtexture t.select (Draw.vec (-60) (-80)) (Draw.rect (1 + 121 * int2Float (index `mod` 8)) (1 + 223 * int2Float (index `div` 8)) 120 160)
        dtexture t.select (Draw.vec (-40) (-30)) (Draw.rect (1 + 81 * int2Float (index `mod` 8)) (162 + 223 * int2Float (index `div` 8)) 80 30)
        when (index > 0) $ dtexture t.select (Draw.vec (-60) (-20)) (Draw.rect 649 162 13 11)
        when (index < 15) $ dtexture t.select (Draw.vec 47 (-20)) (Draw.rect 663 162 13 11)
        dtext (printf "%02d/16" (index + 1)) (Draw.vec 0 (-75)) True
        dtext (printf "HiScore:XXXX") (Draw.vec 0 (-60)) True
        dtext (maybe "????" (`replicate` '*') music.difficulty) (Draw.vec 0 (-45)) True
        dtext "-TITLE--------" (Draw.vec 0 5) True
        dtext music.name (Draw.vec (-56) 15) False
        dtext "-ARTIST-------" (Draw.vec 0 35) True
        dtext music.artist (Draw.vec (-56) 45) False
        dtext (printf "BPM:%d" (round music.bpm :: Int)) (Draw.vec (-56) 65) False
        return ()
      _ -> return ()

shouldClose :: StateT Game IO Bool
shouldClose = use close

teardown :: StateT Game IO ()
teardown = do
  w <- use window
  lift $ closeWindow $ Just w