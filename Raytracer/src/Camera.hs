module Camera where

import LinearAlgebra
import Raster

data Camera = Camera {
    camPos :: Vector,
    camDir :: Vector,
    camUp :: Vector,
    camWidthAngleDegrees :: Double -- TODO: dir and up must be perpendicular, and angle must be less than 90
}

defaultCamera = Camera {
    camPos = (Vector 0 0 (-5)),
    camDir = (Vector 0 0 1),
    camUp = (Vector 0 1 0),
    camWidthAngleDegrees = 60
}

pixelRays :: Camera -> Canvas -> [[Ray]]
pixelRays camera canvas = map (map (makeCameraRay camera)) (pixelVectors camera canvas)
    where makeCameraRay camera vector = Ray (camPos camera) (normalized vector)

pixelVectors :: Camera -> Canvas -> [[Vector]]
pixelVectors camera (Canvas resWidth resHeight) =
    map vecRow [0..(resHeight-1)]
        where vecRow y = map (pixelVector camPlane y) [0..(resWidth-1)]
              camPlane = camPlaneInfo camera (Canvas resWidth resHeight)

camPlaneInfo :: Camera -> Canvas -> CamPlane
camPlaneInfo camera canvas =
    CamPlane {
        planeTopLeft = (topCenter `vecPlus` (cLeft `vecDMult` (0.5 * widthUnitDimension))) `vecPlus` centeringVector,
        cRight = cLeft `vecIMult` (-1),
        cDown = (camUp camera) `vecIMult` (-1),
        planeUnitsPerPixel = unitsPerPixel
    }
        where topCenter = center `vecPlus` ((camUp camera) `vecDMult` (0.5 * heightUnitDimension))
              centeringVector = (Vector (0.5*unitsPerPixel) (-0.5*unitsPerPixel) 0)
              center = camDir camera
              heightUnitDimension = unitsPerPixel * (fromIntegral (heightPixels canvas))
              unitsPerPixel = widthUnitDimension / (fromIntegral (widthPixels canvas))
              widthUnitDimension = 2 * tan(widthAngleRadians / 2)
              widthAngleRadians = (camWidthAngleDegrees camera) * pi / 180
              cLeft = (camDir camera) `cross` (camUp camera)


pixelVector :: CamPlane -> Int -> Int -> Vector
pixelVector (CamPlane planeTopLeft cRight cDown unitsPerPixel) y x = planeTopLeft `vecPlus` (horizOffset `vecPlus` vertOffset)
    where horizOffset = (cRight `vecDMult` (unitsPerPixel * (fromIntegral x)))
          vertOffset = (cDown `vecDMult` (unitsPerPixel * (fromIntegral y)))

data CamPlane = CamPlane {
    planeTopLeft :: Vector,
    cRight :: Vector,
    cDown :: Vector,
    planeUnitsPerPixel :: Double
} deriving (Show)


