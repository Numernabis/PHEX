module ImageConversion where

import Codec.Picture
import Control.Monad
import Control.Monad.ST
import System.Environment (getArgs)
import Data.Word
import Data.Array.Repa hiding ((++))
import Data.Array.Repa.Repr.ForeignPtr
import qualified Data.Array.Repa as R -- for Repa

type RGB8 = (Pixel8, Pixel8, Pixel8)
{-
-- produce delayed Repa array from image with true color pixels.
fromImage :: Image PixelRGB8 -> Array D DIM2 RGB8
fromImage img =
    R.fromFunction
    (Z :. imageWidth :. imageHeight)
    (\(Z :. x :. y) ->
        let (PixelRGB8 r g b) = pixelAt img x y
        in (r, g, b))

-- get image with true color pixels from manifest Repa array.
toImage :: Array U DIM2 RGB8 -> Image PixelRGB8
toImage a = generateImage gen width height
    where
    Z :. width :. height = R.extent a
    gen x y =
        let (r,g,b) = a ! (Z :. x :. y)
        in PixelRGB8 r g b


-- (parallel) desaturation of an image to grey scale
-- Y = 0.21*R + 0.71*G + 0.07*B   or 
-- Y = 0.299*R + 0.587*G + 0.114*B
toGreyScale :: (DIM3 -> Word8) -> DIM3 -> Word8
toGreyScale _ (Z :. _ :. _ :. 3) = 255   -- alpha channel
toGreyScale f (Z :. i :. j :. _) = ceiling $ 0.21 * r + 0.71 * g + 0.07 * b
    where
        r = fromIntegral $ f (Z :. i :. j :. 0)
        g = fromIntegral $ f (Z :. i :. j :. 1)
        b = fromIntegral $ f (Z :. i :. j :. 2)


-- (parallel) desaturation of an image to sepia scale
-- first convert to grey scale
toSepiaScale :: (DIM3 -> Word8) -> DIM3 -> Word8
toSepiaScale _ (Z :. _ :. _ :. 3) = 255   -- alpha channel
toSepiaScale f (Z :. i :. j :. _) = ceiling $ 1 * r + 0.95 * g + 0.82 * b
    where
        r = fromIntegral $ f (Z :. i :. j :. 0)
        g = fromIntegral $ f (Z :. i :. j :. 1)
        b = fromIntegral $ f (Z :. i :. j :. 2)


imageC = do
    [f] <- getArgs
    runIL $ do
    (RGB a) <- readImage f
    b <- (computeP $ traverse a id toGreyScale) :: IL (Array F DIM3 Word8)
    writeImage ("grey-" ++ f) (RGB b)
    c <- (computeP $ traverse b id toSepiaScale) :: IL (Array F DIM3 Word8)
    writeImage ("sepia-" ++ f) (RGB c)

imageCon = do
    [pathIn, pathOut] <- getArgs
    imgIn <- readImage pathIn
    case imgIn of
        Left err -> putStrLn ("Could not read image: " ++ err)
        Right (ImageRGB8 img) -> do
        imgOut <- (computeP $ R.traverse (fromImage img) id toGreyScale)
        -- imgOut <- (R.computeUnboxedP . toGreyScale . fromImage) img
        (savePngImage pathOut . ImageRGB8 . toImage) imgOut
        Right _ -> putStrLn "Unexpected pixel format"

-}
