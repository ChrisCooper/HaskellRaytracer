module PPM where

import Raster


imageDataToPPM :: ImageData -> String
imageDataToPPM iData = ppmHeader iData ++ ppmBody iData

ppmHeader :: ImageData -> String
ppmHeader iData = "P3\n" ++ show w ++ " " ++ show h ++ "\n" ++ show maxColorValue ++ "\n"
    where h = length (valueRows iData)
          w = length (head (valueRows iData))

ppmBody :: ImageData -> String
ppmBody iData = unlines (map rowString (valueRows iData))
    where rowString colorRow = unwords (map pixelString colorRow)

