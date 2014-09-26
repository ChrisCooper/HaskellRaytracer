module Trace where

import Data.Foldable
import LinearAlgebra
import World
import Raster

traceRayColor :: World -> Ray -> Color
traceRayColor world ray = case (closestIntersection ray (wObjects world)) of
    Just intersection -> grey (max 0 ((iNormal intersection) `dot ` vecUp))
    Nothing -> (wFogColor world)


closestIntersection :: Ray -> [Sphere] -> Maybe Intersection
closestIntersection ray spheres = minimumBy intersectionOrd (map (intersectT ray) spheres)

intersectionOrd :: Maybe Intersection -> Maybe Intersection -> Ordering
intersectionOrd Nothing (Just bInter) = GT
intersectionOrd (Just aInter) Nothing = LT
intersectionOrd (Just aInter) (Just bInter) = if (tValue aInter) < (tValue bInter)
    then LT
    else GT
intersectionOrd _ _ = EQ

