module Game.Draw (Vector, Rectangle, Texture, vec, (|+|), (|-|), (|*), magnitude, rect, texture, text) where

import Data.Char (ord)
import Data.List.Extra (splitOn)
import GHC.Float (int2Float)
import Game.Config (Config (..))
import qualified Raylib.Core.Textures as Raylib
import qualified Raylib.Types as Raylib
import qualified Raylib.Util.Colors as Colors
import qualified Raylib.Util.Math as Math

type Vector = Raylib.Vector2

vec :: Float -> Float -> Raylib.Vector2
vec = Raylib.Vector2

(|+|), (|-|) :: Vector -> Vector -> Vector
(|+|) = (Math.|+|)
(|-|) = (Math.|-|)

(|*) :: Vector -> Float -> Vector
(|*) = (Math.|*)

magnitude :: Vector -> Float
magnitude = Math.magnitude

type Rectangle = Raylib.Rectangle

rect :: Float -> Float -> Float -> Float -> Raylib.Rectangle
rect = Raylib.Rectangle

type Texture = Raylib.Texture

texture :: Config -> Texture -> Vector -> Rectangle -> IO ()
texture config t (Raylib.Vector2 x y) (Raylib.Rectangle srcx srcy w h) =
  Raylib.drawTexturePro t (rect (srcx + 0.1) (srcy + 0.1) (w - 0.1) (h - 0.1)) dst (Raylib.Vector2 0 0) 0 Colors.white
  where
    dst = Raylib.Rectangle (config.actualWidth / 2 + scale * x) (config.actualHeight / 2 + scale * y) (scale * w) (scale * h)
    scale = if config.actualWidth / config.actualHeight < 120 / 160 then config.actualWidth / 120 else config.actualHeight / 160

text :: (Texture -> Vector -> Rectangle -> IO ()) -> Texture -> String -> Vector -> Bool -> Bool -> IO ()
text drawer font str (Raylib.Vector2 x y) centering small = mapM_ (\(offsety, line) -> mapM_ (f offsety) $ zip [0 ..] line) $ zip [0 ..] $ splitOn "\n" str
  where
    f offsety (offsetx, char) = drawer font (vec (x + (w + if small then 1 else 0) * offsetx - if centering then int2Float (length str) * w / 2 else 0) (y + (h + 1) * offsety)) (rect (1 + (w + 1) * int2Float (c `mod` 32)) (startY + (h + 1) * int2Float (c `div` 32)) w h)
      where
        (w, h, startY) = if small then (5, 7, 31) else (8, 9, 1)
        c = ord char - 0x20