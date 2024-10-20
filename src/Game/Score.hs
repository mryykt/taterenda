module Game.Score (Score, write, read) where

import Control.Monad.Extra (ifM)
import Data.Aeson.Micro
  ( FromJSON (..)
  , ToJSON (..)
  , decodeStrict
  , encodeStrict
  )
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (pack, unpack)
import Raylib.Core (fileExists)
import Prelude hiding (read)

newtype Score = Score (Map String Int)

instance FromJSON Score where
  parseJSON = fmap (Score . Map.mapKeys unpack) . parseJSON

instance ToJSON Score where
  toJSON (Score d) = toJSON (Map.mapKeys pack d)

write :: Score -> IO ()
write = BS.writeFile "score.json" . encodeStrict

read :: IO Score
read =
  ifM
    (fileExists "score.json")
    (fromMaybe (error "score file is invalid") . decodeStrict <$> BS.readFile "score.json")
    (write def >> return def)

def :: Score
def = Score Map.empty