module Trace where

import Data.Foldable
import LinearAlgebra
import World
import Raster

traceRayColor :: World -> Ray -> Color
traceRayColor world ray = case (closestIntersection ray (wObjects world)) of
    Just intersection -> (Color 0 0 0)
    Nothing -> (wFogColor world)


closestIntersection :: Ray -> [Sphere] -> Maybe Intersection
closestIntersection ray spheres = minimumBy intersectionOrd (map (intersectT ray) spheres)

intersectionOrd :: Maybe Intersection -> Maybe Intersection -> Ordering
intersectionOrd Nothing (Just bInter) = GT
intersectionOrd (Just aInter) Nothing = LT
intersectionOrd (Just aInter) (Just bInter) = EQ -- TODO This works for silhouettes only
intersectionOrd _ _ = EQ

