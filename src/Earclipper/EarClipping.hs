{-| Ear clipping triangulation
-}
module Earclipper.EarClipping
  ( toTriangle
  , triangulate
  , pointInTriangle
  , isAnyPointInTriangle
  , isConvex
  ) where

import Data.List (any, length)

-- | Triangulate given polygon and return a list of triangles.  The given
-- polygon's start point end end point has to be equal.  The first point is
-- removed before triangulation.  Use the triangulate function if you want to
-- supply a polygon directly without modification.
toTriangle :: [(Double, Double)] -> [[(Double, Double)]]
toTriangle [] = []
toTriangle (_:bounds) =
  triangulate bounds

-- | Triangulate given polygon and return a list of triangles.
-- It is assumed that the given polygon's end point is the start point
-- and that the first point in the list is not the start point.
triangulate :: [(Double, Double)] -> [[(Double, Double)]]
triangulate =
  triangulateEar 0

-- | Ear clipping triangulation
triangulateEar :: Int -> [(Double, Double)] -> [[(Double, Double)]]
triangulateEar _ []  = []
triangulateEar _ [_]  = []
triangulateEar _ [_, _]  = []
triangulateEar _ [a, x, c]  = [[a, x, c]]
triangulateEar lastear (a:x:c:xs)
  | lastear > 2*size = [[a,x,c]]
  | earfound = [a, x, c] : triangulateEar 0 ([a]++[c]++xs)
  | otherwise = triangulateEar (lastear+1) ([x,c] ++ xs ++ [a])
  where earfound = convex && noPointInTriangle
        noPointInTriangle = not $ isAnyPointInTriangle [a, x, c] xs
        convex = isConvex a x c
        size = 3 + length xs

-- | Check if given point is in given triangle.
pointInTriangle :: [(Double, Double)] -> (Double, Double) -> Bool
pointInTriangle [(ax, ay), (bx, by), (cx, cy)] (px, py) 
  | b0 == 0 = False
  | otherwise = (b1 > 0) && (b2 > 0) && (b3 > 0)
  where b0 = (bx - ax) * (cy - ay) - (cx - ax) * (by - ay)
        b1 = ((bx - px) * (cy - py) - (cx - px) * (by - py))/b0
        b2 = ((cx - px) * (ay - py) - (ax - px) * (cy - py))/b0
        b3 = 1.0 - b1 - b2
pointInTriangle _ _ = False

-- | Check if any given point in list is in the given triangle.
isAnyPointInTriangle :: [(Double, Double)] -> [(Double, Double)] -> Bool
isAnyPointInTriangle triangle =
  any (pointInTriangle triangle)

-- | Check if given points are convex.
isConvex :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
isConvex (p1x, p1y) (px, py) (p2x, p2y)  =
  l < 0
  where
        l = (p1x - px) * (p2y - py) - (p1y - py) * (p2x - px)
