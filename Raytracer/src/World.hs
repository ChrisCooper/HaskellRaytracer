module World where

import LinearAlgebra
import Camera
import Raster

makeWorld :: String -> World
makeWorld _ = defaultWorld

data World = World {
    wCamera :: Camera,
    wObjects :: [Sphere],
    wFogColor :: Color
}

defaultWorld = World {
    wCamera = defaultCamera,
    wObjects = [defaultSphere],
    wFogColor = Color 230 230 230
}
