module Game.Transition (Transition, init, get, to, update) where

import Game.Draw (Vector, magnitude, (|*), (|+|), (|-|))
import Prelude hiding (init)

data Transition = Transition Vector (Maybe (Vector, Vector))

init :: Vector -> Transition
init v = Transition v Nothing

get :: Transition -> Vector
get (Transition v _) = v

to :: Vector -> Vector -> Transition -> Maybe Transition
to dest vel (Transition p Nothing) = Just $ Transition p (Just (dest, vel))
to _ _ _ = Nothing

update :: Float -> Transition -> Transition
update dt (Transition pos (Just (dest, vel)))
  | magnitude (next |-| dest) < 0.5 = Transition dest Nothing
  | otherwise = Transition next (Just (dest, vel))
  where
    next = pos |+| (vel |* dt)
update _ anim = anim