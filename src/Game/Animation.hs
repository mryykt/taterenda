module Game.Animation (Animation, update) where

import Game.Draw (Vector, magnitude, (|*), (|+|), (|-|))

data Animation = Animation Vector (Maybe (Vector, Vector))

update :: Float -> Animation -> Animation
update dt (Animation pos (Just (dest, vel)))
  | magnitude (next |-| dest) < 5 = Animation dest Nothing
  | otherwise = Animation next (Just (dest, vel))
  where
    next = pos |+| (vel |* dt)
update _ anim = anim