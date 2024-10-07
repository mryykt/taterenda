module Game.Draw (Vector, Rectangle, Texture, vec, rect, texture, text) where

import Data.Char (ord)
import GHC.Float (int2Float)
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

text :: (Texture -> Vector -> Rectangle -> IO ()) -> Texture -> String -> Vector -> Bool -> IO ()
text drawer font str (Raylib.Vector2 x y) centering = mapM_ f $ zip [0 ..] str
  where
    f (offset, char) = drawer font (vec (x + 8 * offset - if centering then int2Float (length str * 8) / 2 else 0) y) (rect (1 + 9 * int2Float (c `mod` 32)) (1 + 10 * int2Float (c `div` 32)) 8 9)
      where
        c = ord char - 0x20
