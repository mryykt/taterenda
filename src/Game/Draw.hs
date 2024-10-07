module Game.Draw (Vector, Rectangle, Texture, vec, rect, texture) where

import Game.Config (Config (..))
import qualified Raylib.Core.Textures as Raylib
import qualified Raylib.Types as Raylib
import qualified Raylib.Util.Colors as Colors

type Vector = Raylib.Vector2

vec :: Float -> Float -> Raylib.Vector2
vec = Raylib.Vector2

type Rectangle = Raylib.Rectangle

rect :: Float -> Float -> Float -> Float -> Raylib.Rectangle
rect = Raylib.Rectangle

type Texture = Raylib.Texture

texture :: Config -> Texture -> Vector -> Rectangle -> IO ()
texture config t (Raylib.Vector2 x y) src@(Raylib.Rectangle _ _ w h) =
  Raylib.drawTexturePro t src dst (Raylib.Vector2 0 0) 0 Colors.white
  where
    dst = Raylib.Rectangle (config.actualWidth / 2 + scale * x) (config.actualHeight / 2 + scale * y) (scale * w) (scale * h)
    scale = if config.actualWidth / config.actualHeight < 120 / 160 then config.actualWidth / 120 else config.actualHeight / 160