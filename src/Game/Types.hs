module Game.Types (module Game.Types) where

import Data.IntMap (IntMap)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Game.Animation (Animation)
import Game.Config (Config, EditMode)
import Game.Draw (Rectangle, Texture, Vector)
import Game.Resource (Loader)
import Game.Scores (JudgementCount, Scores)
import Game.Transition (Transition)
import Lens.Micro (Lens', lens)
import Lens.Micro.TH (makeFields, makeLenses)
import Music (MusicList)
import Raylib.Types (Sound)
import Raylib.Util (WindowResources)
import Tateren.Types (Key, Measure, Note, Tateren)
import Time (HasTime (time), Time)

data Game = Game
  { _window :: WindowResources
  , _config :: Config
  , _scores :: Scores
  , _close :: Bool
  , _drawer :: (Texture -> Vector -> Rectangle -> IO (), String -> Vector -> Bool -> Bool -> IO ())
  , _musicList :: MusicList
  , _textures :: Textures
  , _appState :: AppState
  }

data Textures = Textures
  { font :: Texture
  , select :: Texture
  , skin :: Texture
  , title :: Texture
  }

data AppState = InitState | TitleState Title | SelectState Select | LoadState Load | PlayState Play | HiScoreState Select | ConfigState EditMode

data Title = Title {_cursor :: TitleCusor, _bar :: Transition}

data TitleCusor = Start | HiScore | Quit deriving (Eq, Ord, Enum)

data Select = Select {_left :: Transition, _right :: Transition}

data Load = Load {_loadTateren :: Tateren, _loadAuto :: Bool, _loadSounds :: Loader IntMap Sound}

data Play = Play
  { _playTime :: Time
  , _playGauge :: Float
  , _playCurrentBpm :: Float
  , _playStop :: Maybe Time
  , _playTateren :: Tateren
  , _playPlayNotes :: Map Key [Note]
  , _playPlayMeasures :: [Measure]
  , _playKeys :: Set Key
  , _playPlaySounds :: [Sound]
  , _playJudgement :: Maybe Animation
  , _playBombs :: Map Key Animation
  , _playJudgementCount :: JudgementCount
  , _playTotalNotesCount :: Int
  , _playAuto :: Bool
  , _playSounds :: IntMap Sound
  }

makeLenses ''Game
makeLenses ''Title
makeFields ''Load
makeFields ''Play
makeLenses ''JudgementCount

titleState :: Lens' AppState Title
titleState = lens getter setter
  where
    getter (TitleState x) = x
    getter _ = error "error"
    setter (TitleState _) y = TitleState y
    setter _ _ = undefined

playState :: Lens' AppState Play
playState = lens getter setter
  where
    getter (PlayState x) = x
    getter _ = undefined
    setter (PlayState _) y = PlayState y
    setter _ _ = undefined

isConfigState :: AppState -> Bool
isConfigState (ConfigState _) = True
isConfigState _ = False