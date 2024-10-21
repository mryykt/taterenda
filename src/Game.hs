{-# LANGUAGE MultiWayIf #-}

module Game (mainLoop) where

import Control.Monad.Extra (forM_, ifM, notM, orM, unless, when, whenJust, whenJustM, whenM, whileM)
import Control.Monad.State.Strict
  ( StateT
  , evalStateT
  , lift
  )
import Data.IntMap ((!?))
import Data.List.Extra (firstJust, foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Set as Set
import GHC.Float (int2Float)
import qualified Game.Animation as Animation
import Game.Config (Config (..))
import qualified Game.Config as Config
import Game.Draw ((|+|), (|-|))
import qualified Game.Draw as Draw
import qualified Game.Resource as Resource
import qualified Game.Scores as Scores
import qualified Game.Transition as Transition
import Game.Types
  ( AppState (..)
  , Game (Game)
  , HasGauge (gauge)
  , JudgementCount (JudgementCount)
  , Load (..)
  , Play (Play)
  , Select (Select)
  , Textures (..)
  , Title (Title)
  , TitleCusor (..)
  , appState
  , bad
  , bar
  , bombs
  , close
  , currentBpm
  , cursor
  , drawer
  , exScore
  , good
  , great
  , judgement
  , judgementCount
  , keys
  , musicList
  , playMeasures
  , playNotes
  , playSounds
  , playState
  , poor
  , scores
  , sounds
  , stop
  , tateren
  , textures
  , titleState
  , totalNotesCount
  , window
  )
import Lens.Micro (Lens', (^.))
import Lens.Micro.Mtl (use, zoom, (%=), (+=), (-=), (.=), (<%=))
import Music (bpm)
import qualified Music
import Raylib.Core (clearBackground, closeWindow, getFrameTime, getScreenHeight, getScreenWidth, initWindow, isKeyDown, isKeyPressed, setTargetFPS, setTraceLogLevel, toggleBorderlessWindowed)
import Raylib.Core.Audio (initAudioDevice, playSound, unloadSound)
import Raylib.Types (BlendMode (BlendAdditive), KeyboardKey (KeyDown, KeyEnter, KeyEscape, KeyLeft, KeyLeftShift, KeyRight, KeyUp, KeyX, KeyZ), TraceLogLevel (LogNone))
import Raylib.Util (blendMode, drawing)
import Raylib.Util.Colors (black)
import System.FilePath ((</>))
import qualified Tateren
import Tateren.Types (Key (..), bgms, bpmChanges, key, measures, notes, stops, value)
import Text.Printf (printf)
import Time (HasTime (time))
import qualified Time
import Prelude hiding (init, unzip)

mainLoop :: IO ()
mainLoop = init >>= evalStateT (whileM (notM $ update >> draw >> shouldClose) >> teardown)

init :: IO Game
init = do
  config <- Config.read
  s <- Scores.read
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
  return $ Game w config' s False (drawTexture, Draw.text drawTexture ts.font) Music.list ts InitState

update :: StateT Game IO ()
update = do
  w <- use window
  state <- use appState
  dt <- lift getFrameTime
  case state of
    InitState -> appState .= initTitle
    TitleState tit -> do
      zoom (appState . titleState) $
        do
          whenM (((tit ^. cursor > Start) &&) <$> orM [lift $ isKeyPressed KeyUp, lift $ isKeyPressed KeyLeft]) $ do
            b <- bar %?= Transition.to (Transition.get (tit ^. bar) |-| Draw.vec 0 13) (Draw.vec 0 (-50))
            when b $ cursor %= pred
          whenM (((tit ^. cursor < Quit) &&) <$> orM [lift $ isKeyPressed KeyDown, lift $ isKeyPressed KeyRight]) $ do
            b <- bar %?= Transition.to (Transition.get (tit ^. bar) |+| Draw.vec 0 13) (Draw.vec 0 50)
            when b $ cursor %= succ
          bar %= Transition.update dt
      whenM
        (lift $ isKeyPressed KeyEnter)
        $ case tit ^. cursor of
          Start -> appState .= initSelect
          HiScore -> undefined
          Quit -> close .= True
      whenM (lift $ isKeyPressed KeyEscape) (close .= True)
    SelectState _ -> do
      ifM (lift $ isKeyDown KeyLeftShift) (musicList %= Music.selectAnother) (musicList %= Music.selectNormal)
      whenM
        (lift $ isKeyPressed KeyRight)
        $ musicList %= Music.next
      whenM
        (lift $ isKeyPressed KeyLeft)
        $ musicList %= Music.prev
      whenM
        (lift $ isKeyPressed KeyEnter)
        $ do
          (_, curr) <- Music.current <$> use musicList
          let dir = "sound" </> curr.directory
          tate <- lift $ Tateren.load $ dir </> curr.chart
          loader <- lift $ Resource.soundLoader $ (dir </>) <$> curr.sounds
          appState .= LoadState (Load tate loader)
          return ()
      whenM
        (lift $ isKeyPressed KeyEscape)
        (appState .= initTitle)
    LoadState ld -> do
      (_, music) <- Music.current <$> use musicList
      whenJustM
        (lift $ Resource.get (ld ^. sounds))
        ( \s ->
            appState
              .= PlayState
                ( Play
                    (Time.fromInt 0)
                    0
                    20
                    music.bpm
                    Nothing
                    (ld ^. tateren)
                    (Map.fromList [(Sc, []), (K1, []), (K2, [])])
                    []
                    Set.empty
                    []
                    Nothing
                    Map.empty
                    (JudgementCount 0 0 0 0)
                    (length $ ld ^. tateren . notes)
                    s
                )
        )
    PlayState pl -> do
      zoom (appState . playState) $ do
        keys .= Set.empty
        playSounds .= []
        t <- use time

        -- keyboard control
        let
          f k x = if x ^. key == k then Just x else Nothing
          keyHit k = case ((pl ^. playNotes) Map.!? k, firstJust (f k) (pl ^. tateren . notes)) of
            (Just (n : ns), _) -> do
              whenJust ((pl ^. sounds) !? (n ^. value)) (\s -> playSounds %= (s :))
              let
                diff = n ^. time - t
              if
                | abs diff <= Time.fromSeconds (pl ^. currentBpm) (10 / 60) -> do
                    let heal = 800 / (int2Float (pl ^. totalNotesCount) + 700)
                    jt <-
                      if
                        | abs diff <= Time.fromSeconds (pl ^. currentBpm) (1 / 60) -> exScore += 2 >> gauge %= min 100 . (+ heal) >> judgementCount . great += 1 >> return Animation.pgreat
                        | abs diff <= Time.fromSeconds (pl ^. currentBpm) (4 / 60) -> exScore += 1 >> gauge %= min 100 . (+ heal) >> judgementCount . great += 1 >> return Animation.great
                        | otherwise -> gauge %= min 100 . (+ heal / 2) >> judgementCount . good += 1 >> return Animation.good
                    judgement .= Just jt
                    bombs %= Map.insert k Animation.bomb
                    playNotes %= Map.insert k ns
                | abs diff <= Time.fromSeconds (pl ^. currentBpm) (23 / 60) -> do
                    gauge -= 2
                    judgement .= Just Animation.bad
                    playNotes %= Map.insert k ns
                    judgementCount . bad += 1
                | diff >= Time.fromSeconds (pl ^. currentBpm) 1 -> return ()
                | otherwise -> gauge -= 2 >> judgementCount . poor += 1 >> judgement .= Just Animation.poor
            (_, Just n) -> whenJust ((pl ^. sounds) !? (n ^. value)) (\s -> playSounds %= (s :))
            (_, Nothing) -> return ()
        whenM (lift $ isKeyPressed KeyLeftShift) (keyHit Sc)
        whenM (lift $ isKeyPressed KeyZ) (keyHit K1)
        whenM (lift $ isKeyPressed KeyX) (keyHit K2)
        whenM (lift $ isKeyDown KeyLeftShift) (keys %= Set.insert Sc)
        whenM (lift $ isKeyDown KeyZ) (keys %= Set.insert K1)
        whenM (lift $ isKeyDown KeyX) (keys %= Set.insert K2)
        --  update
        sounds1 <- (tateren . bgms) >%= Time.get t
        notes1 <- (tateren . notes) >%= Time.get (t + lengthInDisplay)
        measures1 <- (tateren . measures) >%= Time.get (t + lengthInDisplay)
        bpmChanges1 <- (tateren . bpmChanges) >%= Time.get t
        stops1 <- (tateren . stops) >%= Time.get t
        let insertNote ns n = Map.update (Just . (++ [n])) (n ^. key) ns
        playNotes %= flip (foldl' insertNote) notes1
        playMeasures %= ((++ measures1) . dropWhile ((t >) . (^. time)))
        currentBpm %= (\b -> maybe b (int2Float . (^. value)) $ listToMaybe bpmChanges1)
        case pl ^. stop of
          Just st | st > 0 -> stop .= Just (Time.update (-dt) (pl ^. currentBpm) st)
          _ -> do
            stop .= listToMaybe (Time.fromInt . (^. value) <$> stops1)
            time %= Time.update dt (pl ^. currentBpm)
        judgement %= (Animation.update dt =<<)
        bombs %= (\b -> foldl' (flip (Map.update (Animation.update dt))) b [Sc, K1, K2])
        poors <- playNotes >%= (unzip . fmap (Time.get (t - Time.fromSeconds (pl ^. currentBpm) (23 / 60))))
        unless (all null poors) (gauge -= 6 >> judgementCount . poor += 1 >> judgement .= Just Animation.poor)
        playSounds %= (mapMaybe (((pl ^. sounds) !?) . (^. value)) sounds1 ++)
      when (all null (pl ^. playNotes) && null (pl ^. tateren . notes) && null (pl ^. tateren . bgms)) $ do
        cm <- Music.current <$> use musicList
        score' <- scores <%= Scores.update (snd cm) (pl ^. exScore)
        lift $ Scores.write score'
        appState .= initSelect
      whenM (lift $ isKeyPressed KeyEscape) (lift (mapM_ (`unloadSound` w) (pl ^. sounds)) >> appState .= initSelect)

initTitle :: AppState
initTitle = TitleState (Title Start (Transition.init (Draw.vec (-40) 40)))

initSelect :: AppState
initSelect = SelectState (Select (Transition.init (Draw.vec 0 0)) (Transition.init (Draw.vec 0 0)))

(%?=) :: Lens' a b -> (b -> Maybe b) -> StateT a IO Bool
l %?= f = do
  x <- use l
  let y = f x
  case y of
    Just y' -> l .= y' >> return True
    Nothing -> return False

(>%=) :: Lens' a b -> (b -> (b, b)) -> StateT a IO b
l >%= f = do
  x <- use l
  let (y1, y2) = f x
  l .= y2 >> return y1

unzip :: (Functor f) => f (a, b) -> (f a, f b)
unzip xs = (fst <$> xs, snd <$> xs)

draw :: StateT Game IO ()
draw = do
  (dtexture, dtext) <- use drawer
  t <- use textures
  ml <- use musicList
  state <- use appState
  s <- use scores
  lift $ drawing $ do
    clearBackground black
    case state of
      TitleState tit -> do
        dtexture t.title (Draw.vec (-60) (-80)) (Draw.rect 1 1 120 160)
        dtexture t.font (Transition.get $ tit ^. bar) (Draw.rect 199 31 80 9)
        dtext "START" (Draw.vec 0 40) True False
        dtext "HISCORE" (Draw.vec 0 53) True False
        dtext "QUIT" (Draw.vec 0 66) True False
      SelectState _ -> do
        let (index, music) = Music.current ml
        dtexture t.select (Draw.vec (-60) (-80)) (Draw.rect (1 + 121 * int2Float (index `mod` 8)) (1 + 223 * int2Float (index `div` 8)) 120 160)
        dtexture t.select (Draw.vec (-40) (-30)) (Draw.rect (1 + 81 * int2Float (index `mod` 8)) (162 + (if Music.isAnother ml then 31 else 0) + 223 * int2Float (index `div` 8)) 80 30)
        when (index > 0) $ dtexture t.select (Draw.vec (-60) (-20)) (Draw.rect 649 162 13 11)
        when (index < 15) $ dtexture t.select (Draw.vec 47 (-20)) (Draw.rect 663 162 13 11)
        dtext (printf "%02d/16" (index + 1)) (Draw.vec 0 (-75)) True False
        whenJust (Scores.get music s) $ \v -> dtext (printf "HiScore:%4d" v) (Draw.vec 0 (-60)) True False
        dtext (maybe "????" (`replicate` '*') music.difficulty) (Draw.vec 0 (-45)) True False
        dtext "-TITLE--------" (Draw.vec 0 5) True False
        dtext music.name (Draw.vec (-56) 15) False False
        dtext "-ARTIST-------" (Draw.vec 0 35) True False
        dtext music.artist (Draw.vec (-56) 45) False False
        dtext (printf "BPM:%d" (round music.bpm :: Int)) (Draw.vec (-56) 65) False False
      LoadState _ -> dtexture t.title (Draw.vec (-60) (-80)) (Draw.rect 122 1 120 160)
      PlayState pl -> do
        dtexture t.skin (Draw.vec (-60) (-80)) (Draw.rect 1 1 120 160)
        forM_ (pl ^. keys) $ \k -> do
          let (x, range) = case k of
                Sc -> (9 - 60, Draw.rect 122 1 16 139)
                K1 -> (26 - 60, Draw.rect 139 1 9 139)
                K2 -> (36 - 60, Draw.rect 139 1 9 139)
          dtexture t.skin (Draw.vec x (-80)) range
        let y ot = (141 - (141 / lengthInDisplay * Time.toFloat (ot - pl ^. time))) - 82
        forM_ (pl ^. playMeasures) $ \m -> do
          dtexture t.skin (Draw.vec (8 - 60) (y (m ^. time) + 1)) (Draw.rect 9 162 38 1)
        forM_ (pl ^. playNotes) $
          mapM_
            ( \n -> do
                let (x, range) = case n ^. key of
                      Sc -> (9 - 60, Draw.rect 122 141 16 2)
                      K1 -> (26 - 60, Draw.rect 139 141 9 2)
                      K2 -> (36 - 60, Draw.rect 139 141 9 2)
                dtexture t.skin (Draw.vec x (y (n ^. time))) range
            )
            . dropWhile (((pl ^. time) >) . (^. time))
        whenJust (pl ^. judgement) $ Animation.draw dtexture t.skin (Draw.vec (-52) 20)
        let drawBomb k x = whenJust ((pl ^. bombs) Map.!? k) $ Animation.draw dtexture t.skin (Draw.vec x 56)
        drawBomb Sc (13 - 60)
        drawBomb K1 (27 - 60)
        drawBomb K2 (37 - 60)
        blendMode
          BlendAdditive
          ( do
              forM_ [0 .. round (pl ^. gauge * 31 / 100) - 1] $ \i ->
                dtexture t.skin (Draw.vec (-5 + 2 * int2Float i) 63) $ if i > 23 then Draw.rect 124 144 1 8 else Draw.rect 122 144 1 8
              forM_ [(pl ^. judgementCount . great, -57), (pl ^. judgementCount . good, -33), (pl ^. judgementCount . bad, -9), (pl ^. judgementCount . poor, 15), (pl ^. exScore, 39)] $ \(c, y') -> do
                dtext (printf "%4d" c) (Draw.vec 15 y') False True
          )
        mapM_ playSound (pl ^. playSounds)
      _ -> return ()

lengthInDisplay :: (Fractional t) => t
lengthInDisplay = 0xc0 * 1.5

shouldClose :: StateT Game IO Bool
shouldClose = use close

teardown :: StateT Game IO ()
teardown = do
  w <- use window
  lift $ closeWindow $ Just w