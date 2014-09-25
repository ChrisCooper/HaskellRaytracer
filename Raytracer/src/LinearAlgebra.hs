module LinearAlgebra where

data Vector = Vector {
    x :: Double,
    y :: Double,
    z :: Double
} deriving (Show)

data Ray = Ray {
    start :: Point,
    direction :: Vector
}

type Point = Vector

data Sphere = Sphere Point Double

defaultSphere = Sphere (Vector 0 0 0) 1

----------------------------------------------------------------------------------------------------
-- Simple Vector Math
----------------------------------------------------------------------------------------------------

vecPlus :: Vector -> Vector -> Vector
vecPlus (Vector x1 y1 z1) (Vector x2 y2 z2) =  Vector (x1+x2) (y1+y2) (z1+z2)

vecMinus :: Vector -> Vector -> Vector
vecMinus (Vector x1 y1 z1) (Vector x2 y2 z2) =  Vector (x1-x2) (y1-y2) (z1-z2)

vecIMult :: Vector -> Int -> Vector
vecIMult (Vector x y z) fact =  Vector (x*dfact) (y*dfact) (z*dfact)
    where dfact = (fromIntegral fact)

vecDMult :: Vector -> Double -> Vector
vecDMult (Vector x y z) fact =  Vector (x*fact) (y*fact) (z*fact)

vecLength :: Vector -> Double
vecLength (Vector x y z) = sqrt (x*x + y*y + z*z)

normalized :: Vector -> Vector
normalized (Vector x y z) = (Vector (x/l) (y/l) (z/l))
    where l = vecLength (Vector x y z)

dot :: Vector -> Vector -> Double
dot (Vector x1 y1 z1) (Vector x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

cross :: Vector -> Vector -> Vector
cross (Vector x1 y1 z1) (Vector x2 y2 z2) = (Vector x3 y3 z3)
    where x3 = y1*z2 - z1*y2
          y3 = z1*x2 - x1*z2
          z3 = x1*y2 - y1*x2

pointDist :: Point -> Point -> Double
pointDist v1 v2 = vecLength (v2 `vecMinus` v1)

----------------------------------------------------------------------------------------------------
-- Intersections
----------------------------------------------------------------------------------------------------

data Intersection = Intersection {
    tValue :: Double,
    location :: Vector
} deriving (Show)

intersectT :: Ray -> Sphere -> Maybe Intersection
intersectT (Ray start dir) (Sphere center radius) = if discriminant < 0
    then Nothing
    else Just (Intersection 1 (Vector 0 0 1))
        where a = dir `dot` dir
              b = 2 * (dir `dot` (start `vecMinus` center))
              c = ((start `vecMinus` center) `dot` (start `vecMinus` center)) - (radius^2)
              discriminant = b^2 - (4 * a * c)






