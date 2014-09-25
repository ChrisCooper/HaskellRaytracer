module Raster where

data Canvas = Canvas {
    widthPixels :: Int,
    heightPixels :: Int
}

data ImageData = ImageData {
    width :: Int,
    valueRows :: [[Color]]
}

data Color = Color {
    r :: Double,
    g :: Double,
    b :: Double
}
maxColorValue = 2^8 - 1

pixelString :: Color -> String
pixelString (Color r g b) = unwords (map (show . round) [r,g,b])
