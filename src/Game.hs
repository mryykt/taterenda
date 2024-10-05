module Game (mainLoop) where

import Control.Monad.Extra (notM, whileM)
import Control.Monad.State.Strict
import Raylib.Core (closeWindow, initWindow, windowShouldClose)
import Raylib.Util (WindowResources)
import Prelude hiding (init)

mainLoop :: IO ()
mainLoop = init >>= evalStateT (whileM (notM $ update >> draw >> shouldClose) >> teardown)

init :: IO WindowResources
init = initWindow 640 480 "taterenda"

update :: StateT WindowResources IO ()
update = return ()

draw :: StateT WindowResources IO ()
draw = return ()

shouldClose :: StateT WindowResources IO Bool
shouldClose = lift windowShouldClose

teardown :: StateT WindowResources IO ()
teardown = do
  w <- get
  lift $ closeWindow $ Just w