module Game.Animation (Animation, init, get, to, update) where

import Game.Draw (Vector, magnitude, (|*), (|+|), (|-|))
import Prelude hiding (init)

data Animation = Animation Vector (Maybe (Vector, Vector))

init :: Vector -> Animation
init v = Animation v Nothing

get :: Animation -> Vector
get (Animation v _) = v

to :: Vector -> Vector -> Animation -> Maybe Animation
to dest vel (Animation p Nothing) = Just $ Animation p (Just (dest, vel))
to _ _ _ = Nothing

update :: Float -> Animation -> Animation
update dt (Animation pos (Just (dest, vel)))
  | magnitude (next |-| dest) < 0.5 = Animation dest Nothing
  | otherwise = Animation next (Just (dest, vel))
  where
    next = pos |+| (vel |* dt)
update _ anim = anim