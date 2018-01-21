module ImageGeneration where

import Codec.Picture
import System.Environment (getArgs)

  
generateImg :: DynamicImage
generateImg = ImageRGB8 (generateImage originalFnc 1200 1200)

originalFnc :: Int -> Int -> PixelRGB8
originalFnc x y =
    let (q, r) = x `quotRem` max 10 y
        s      = fromIntegral . min 0xff
    in PixelRGB8 (s q) (s r) (s (q + r + 30))

-- main method
imageGen = do
    [path] <- getArgs
    savePngImage path generateImg