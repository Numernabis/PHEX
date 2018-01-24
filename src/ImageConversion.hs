module ImageConversion where

import Codec.Picture
import System.Environment (getArgs)
import Data.Time.Clock (diffUTCTime, getCurrentTime)

-- | Conversion of an image to grey scale.
toGreyScale :: Image PixelRGB8 -> Image Pixel8
toGreyScale = pixelMap pixelGrey

-- | Changing colour PixelRGB8 into greyscale Pixel8.
-- For simplicity, greyscale pixels use plain numbers instead of a separate type.
pixelGrey :: PixelRGB8 -> Pixel8
pixelGrey (PixelRGB8 r g b) = (div r 3 + div g 3 + div b 3)

-- | Function casting loaded image into Image PixelRGB8
dynToPix :: DynamicImage -> Image PixelRGB8
dynToPix (ImageRGB8 img) = img

-- | Main function - reads path to file, prepares path for save,
-- converts image and saves it.
imageCon = do
    [pathIn] <- getArgs
    start <- getCurrentTime
    Right imgIn <- readPng pathIn 
    let pathGrey = ("grey-" ++ pathIn)
    let imgGrey = toGreyScale . dynToPix $ imgIn
    writePng pathGrey imgGrey
    end <- getCurrentTime
    putStrLn $ "Image converted to grey scale, saved in " ++ pathGrey
    putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."
    
-- -----------------------------------------------------------