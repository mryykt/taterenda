module Game.Textures
  ( Textures (title, load, white, bar, font, backs, selectNormals, selectAnothers, skin)
  , Skin (back, keyBeamSc, keyBeamNote, sc, note, gaugeGreen, gaugeRed, poor, bad, good, great, pGreat, bomb)
  , init
  ) where

import Control.Monad.Extra ((<=<))
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import GHC.Float (int2Float)
import Game.Resource (loadImage)
import Raylib.Core.Textures (imageFromImage, loadTextureFromImage)
import Raylib.Types (Image, Rectangle (Rectangle), Texture)
import Prelude hiding (init)

data Textures = Textures {title, load, white, bar :: Texture, font, backs, selectNormals, selectAnothers :: Vector Texture, skin :: Skin}

data Skin = Skin {back, keyBeamSc, keyBeamNote, sc, note, gaugeGreen, gaugeRed, poor, bad, good, great :: Texture, pGreat, bomb :: Vector Texture}

init :: IO Textures
init = do
  fontImage <- loadImage "font.bmp"
  selectImage <- loadImage "select.bmp"
  titleImage <- loadImage "title.bmp"
  Textures
    <$> fromImage (Rectangle 1 1 120 160) titleImage
    <*> fromImage (Rectangle 122 1 120 160) titleImage
    <*> fromImage (Rectangle 243 1 120 160) titleImage
    <*> fromImage (Rectangle 199 31 80 9) fontImage
    <*> Vector.generateM (32 * 3) (\i -> fromImage (Rectangle (int2Float (1 + 9 * (i `mod` 32))) (int2Float (1 + 10 * (i `div` 32))) 8 9) fontImage)
    <*> return undefined -- Vector.generateM 16 (\i -> fromImage (Rectangle (int2Float (1 + 120 * i `mod` 8)) (int2Float (1 + 223 * i `div` 8)) 120 160) selectImage)
    <*> return undefined -- Vector.generateM 16 (\i -> fromImage (Rectangle (int2Float (1 + 80 * i `mod` 8)) (int2Float (162 + 223 * i `div` 8)) 80 30) selectImage)
    <*> return undefined -- Vector.generateM 16 (\i -> fromImage (Rectangle (int2Float (1 + 80 * i `mod` 8)) (int2Float (193 + 223 * i `div` 8)) 80 30) selectImage)
    <*> separateSkin

separateSkin :: IO Skin
separateSkin = do
  image <- loadImage "skin.bmp"
  Skin
    <$> fromImage (Rectangle 1 1 120 160) image
    <*> fromImage (Rectangle 122 1 16 139) image
    <*> fromImage (Rectangle 139 1 9 139) image
    <*> fromImage (Rectangle 122 141 16 2) image
    <*> fromImage (Rectangle 139 141 9 139) image
    <*> fromImage (Rectangle 122 144 1 8) image
    <*> fromImage (Rectangle 124 144 1 8) image
    <*> fromImage (Rectangle 149 55 40 8) image
    <*> fromImage (Rectangle 149 46 40 8) image
    <*> fromImage (Rectangle 149 37 40 8) image
    <*> fromImage (Rectangle 149 28 40 8) image
    <*> Vector.generateM 4 (\i -> fromImage (Rectangle 149 (1 + 9 * int2Float i) 40 8) image)
    <*> Vector.generateM 6 (\i -> fromImage (Rectangle 149 (64 + 8 * int2Float i) 8 7) image)

fromImage :: Rectangle -> Image -> IO Texture
fromImage rect = loadTextureFromImage <=< flip imageFromImage rect