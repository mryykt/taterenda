module Game (mainLoop) where

import Control.Monad.Extra (notM, whileM)
import Control.Monad.State.Strict
import Game.Types (Game (Game), window)
import Lens.Micro.Mtl (use)
import Raylib.Core (closeWindow, initWindow, windowShouldClose)
import Prelude hiding (init)

mainLoop :: IO ()
mainLoop = init >>= evalStateT (whileM (notM $ update >> draw >> shouldClose) >> teardown)

init :: IO Game
init = Game <$> initWindow 640 480 "taterenda" <*> return undefined

update :: StateT Game IO ()
update = return ()

draw :: StateT Game IO ()
draw = return ()

shouldClose :: StateT Game IO Bool
shouldClose = lift windowShouldClose

teardown :: StateT Game IO ()
teardown = do
  w <- use window
  lift $ closeWindow $ Just w