module Game.Scores (Scores, write, read, update, get) where

import Control.Monad.Extra (ifM)
import Data.Aeson.Micro
  ( FromJSON (..)
  , ToJSON (..)
  , decodeStrict
  , encodeStrict
  )
import qualified Data.ByteString as BS
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (pack, unpack)
import Music (Music, name)
import qualified Music
import Raylib.Core (fileExists)
import Prelude hiding (read)

newtype Scores = Scores (Map String Int)

instance FromJSON Scores where
  parseJSON = fmap (Scores . Map.mapKeys unpack) . parseJSON

instance ToJSON Scores where
  toJSON (Scores d) = toJSON (Map.mapKeys pack d)

write :: Scores -> IO ()
write = BS.writeFile "score.json" . encodeStrict

read :: IO Scores
read =
  ifM
    (fileExists "score.json")
    (fromMaybe (error "score file is invalid") . decodeStrict <$> BS.readFile "score.json")
    (write def >> return def)

get :: Music -> Scores -> Maybe Int
get music (Scores scores) = scores !? Music.normalizeName music.name

update :: Music -> Int -> Scores -> Scores
update music score (Scores scores) = Scores $ Map.insert name' (maybe id max (scores !? name') score) scores
  where
    name' = Music.normalizeName music.name

def :: Scores
def = Scores Map.empty