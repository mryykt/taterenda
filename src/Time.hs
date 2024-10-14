module Time (Time, update, fromInt, toFloat, get, HasTime (time)) where

import GHC.Float (int2Float)
import Lens.Micro ((^.))
import Lens.Micro.TH (makeFields)

newtype Time = Time Float deriving (Show, Eq, Ord)

instance Num Time where
  (Time t1) + (Time t2) = Time (t1 + t2)
  Time t1 * Time t2 = Time (t1 * t2)
  abs (Time t) = Time (abs t)
  signum (Time t) = Time (signum t)
  fromInteger x = Time $ fromInteger x
  negate (Time t) = Time $ negate t

{- | >>> update 1 60 (Time 0)
Time 192.0
| >>> update 0.5 120 (Time 0)
Time 192.0
-}
update :: Float -> Float -> Time -> Time
update dt bpm (Time t) = Time (t + offset)
  where
    offset = dt * bpm / 60 * (0xc0 / 4)

fromInt :: Int -> Time
fromInt = Time . int2Float

toFloat :: Time -> Float
toFloat (Time t) = t

newtype Dummy = Dummy {_dummyTime :: Time}

makeFields ''Dummy

get :: (HasTime a Time) => Time -> [a] -> ([a], [a])
get t = span ((< t) . (^. time))