module Game.Scores (Scores, Score (..), JudgementCount, pgreat, great, good, bad, poor, initJudge, exScore, isFullCombo, write, read, update, get) where

import Control.Monad.Extra (ifM)
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
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (pack, unpack)
import Lens.Micro.TH (makeLenses)
import Music (Music, name)
import qualified Music
import Raylib.Core (fileExists)
import Prelude hiding (read)

newtype Scores = Scores (Map String Score)

instance FromJSON Scores where
  parseJSON = fmap (Scores . Map.mapKeys unpack) . parseJSON

instance ToJSON Scores where
  toJSON (Scores d) = toJSON (Map.mapKeys pack d)

data Score = Score {clear :: Bool, hard :: Bool, fullCombo :: Bool, judge :: JudgementCount}

instance FromJSON Score where
  parseJSON (Object v) = Score <$> v .: "clear" <*> v .: "hard" <*> v .: "full_combo" <*> v .: "judge"
  parseJSON _ = undefined

instance ToJSON Score where
  toJSON score = object ["clear" .= score.clear, "hard" .= score.hard, "full_combo" .= score.fullCombo, "judge" .= score.judge]

data JudgementCount = JudgementCount {_pgreat :: Int, _great :: Int, _good :: Int, _bad :: Int, _poor :: Int}

instance FromJSON JudgementCount where
  parseJSON (Object v) = JudgementCount <$> v .: "pgreat" <*> v .: "great" <*> v .: "good" <*> v .: "bad" <*> v .: "poor"
  parseJSON _ = undefined

instance ToJSON JudgementCount where
  toJSON j = object ["pgreat" .= j._pgreat, "great" .= j._great, "good" .= j._good, "bad" .= j._bad, "poor" .= j._poor]

initJudge :: JudgementCount
initJudge = JudgementCount 0 0 0 0 0

exScore :: JudgementCount -> Int
exScore j = j._pgreat * 2 + j._great

isFullCombo :: Int -> JudgementCount -> Bool
isFullCombo totalNotesCount j = j._pgreat + j._great + j._good == totalNotesCount

write :: Scores -> IO ()
write = BS.writeFile "score.json" . encodeStrict

read :: IO Scores
read =
  ifM
    (fileExists "score.json")
    (fromMaybe (error "score file is invalid") . decodeStrict <$> BS.readFile "score.json")
    (write def >> return def)

get :: Music -> Scores -> Maybe Score
get music (Scores scores) = scores !? Music.normalizeName music.name

update :: Music -> Bool -> Bool -> Bool -> JudgementCount -> Scores -> Scores
update music clear' hard' fullCombo' judge' (Scores scores) =
  Scores $
    Map.insert
      name'
      (maybe (Score clear' hard' fullCombo' judge') newScore (scores !? name'))
      scores
  where
    newScore old =
      Score
        (max clear' old.clear)
        (max hard' old.hard)
        (max fullCombo' old.fullCombo)
        (if exScore judge' > exScore old.judge then judge' else old.judge)
    name' = Music.normalizeName music.name

def :: Scores
def = Scores Map.empty

makeLenses ''JudgementCount