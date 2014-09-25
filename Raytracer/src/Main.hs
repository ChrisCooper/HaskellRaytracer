module Main (
    main
) where

import System.Environment (getArgs)
import LinearAlgebra
import Camera
import Raster
import PPM
import World
import Trace


main = do
    args <- getArgs
    case args of
        [worldFile, imageFile] -> do
            input <- readFile worldFile
            writeFile imageFile (worldStrToPPM input)
        _ -> putStrLn "error: exactly two arguments needed (worldFile, outputFilePPM)"


worldStrToPPM :: String -> String
worldStrToPPM worldStr = imageDataToPPM (renderWorld (makeWorld worldStr))

renderWorld :: World -> ImageData
renderWorld world = ImageData 0 (map (map (traceRayColor world)) cameraRays) -- start with just spheres, then abstract to typeclass
    where cameraRays = pixelRays (wCamera world) (Canvas 500 500)


----------------------------------------------------------------------------------------------------
-- TODO
--
-- Give vectors operators with Num typeclass
--
-- Remove nesting in ImageData, camera ray list, etc. (splitEvery)
--
-- use newtype on Point, Radius, degrees, etc.
