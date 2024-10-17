module Game.Animation (Animation, init, update, draw, pgreat, great, good, bad, poor) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import GHC.Float (int2Float)
import Game.Draw (Rectangle, Texture, Vector)
import qualified Game.Draw as Draw
import Prelude hiding (init)

data Animation = Animation Float (NonEmpty Rectangle) Float Int (NonEmpty Rectangle)

init :: Float -> Int -> [Rectangle] -> Animation
init duration loop frames = Animation 0 (NE.fromList frames) duration loop (NE.fromList frames)

update :: Float -> Animation -> Maybe Animation
update dt (Animation elpased remains duration loop frames)
  | elpased + dt < duration = Just $ Animation (elpased + dt) remains duration loop frames
  | otherwise = case remains of
      _ :| [] | loop > 0 -> Just $ Animation (elpased + dt - duration) frames duration (loop - 1) frames
      _ :| [] -> Nothing
      _ :| (r : rs) -> Just $ Animation (elpased + dt - duration) (r :| rs) duration loop frames

draw :: (Texture -> Vector -> Rectangle -> IO ()) -> Texture -> Vector -> Animation -> IO ()
draw drawer t v (Animation _ (frame :| _) _ _ _) = drawer t v frame

pgreat, great, good, bad, poor :: Animation
pgreat = init 0.03 8 $ map (\i -> Draw.rect 149 (1 + int2Float i * 9) 40 8) [0 .. 3]
great = init 1 0 [Draw.rect 149 28 40 8]
good = init 1 0 [Draw.rect 149 37 40 8]
bad = init 1 0 [Draw.rect 149 46 40 8]
poor = init 1 0 [Draw.rect 149 55 40 8]