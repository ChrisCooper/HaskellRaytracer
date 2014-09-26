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
worldStrToPPM = imageDataToPPM . renderWorld . makeWorld

renderWorld :: World -> ImageData
renderWorld world = ImageData 0 (map (map (traceRayColor world)) cameraRays) -- start with just spheres, then abstract to typeclass
    where cameraRays = pixelRays (wCamera world) (Canvas 1600 900)


----------------------------------------------------------------------------------------------------
-- TODO
--
-- Give Vector operators with Num typeclass
--
-- Give Intersection operators with Ord typeclass
--
-- Remove nesting in ImageData, camera ray list, etc. (use "splitEvery" to seperate rows)
--
-- use newtype on Point, Radius, degrees, etc.
