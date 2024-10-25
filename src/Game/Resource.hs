module Game.Resource (Loader, get, soundLoader, loadTexture) where

import Codec.Picture (DynamicImage, Image (..))
import qualified Codec.Picture as Juicy
import Control.Concurrent.Async (Async, async, mapConcurrently, poll)
import Control.Monad.Extra ((<=<))
import qualified Data.Vector.Storable as Vector
import Raylib.Core.Audio (loadSound)
import qualified Raylib.Core.Textures as Textures
import Raylib.Types (Image (..), PixelFormat (PixelFormatUncompressedR8G8B8), Sound, Texture)
import qualified Raylib.Types as Raylib
import System.FilePath ((</>))

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
soundLoader = loader f
  where
    f = loadSound

convert :: DynamicImage -> Raylib.Image
convert src =
  Raylib.Image
    { image'data = Vector.toList $ imageData img
    , image'width = imageWidth img
    , image'height = imageHeight img
    , image'mipmaps = 1
    , image'format = PixelFormatUncompressedR8G8B8
    }
  where
    img = Juicy.convertRGB8 src

loadTexture :: FilePath -> IO Texture
loadTexture = Textures.loadTextureFromImage . convert . either error id <=< Juicy.readImage . ("image" </>)