module Game.Resource (Loader, get, soundLoader) where

import Control.Concurrent.Async (Async, async, mapConcurrently, poll)
import Raylib.Core.Audio (loadSound)
import Raylib.Types.Core.Audio (Sound)

newtype Loader t a = Loader (Async (t a))

loader :: (Traversable t) => (x -> IO a) -> t x -> IO (Loader t a)
loader f res = do
  th <- async (mapConcurrently f res)
  return $ Loader th

get :: Loader t a -> IO (Maybe (t a))
get (Loader l) = do
  result <- poll l
  case result of
    Just (Right x) -> return $ Just x
    _ -> return Nothing

soundLoader :: (Traversable t) => t String -> IO (Loader t Sound)
soundLoader = loader loadSound
