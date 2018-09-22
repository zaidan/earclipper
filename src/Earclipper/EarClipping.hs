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

-- | Triangulate given polygon and return a list of triangles.  
-- The polygon must be given anticlockwise.  
-- The given polygon's start point and end point have to be equal.
-- The first point is removed before triangulation.
-- Use the triangulate function if you want to supply a polygon directly without modification.  
-- usage example:  
--
-- > toTriangle [(1,-1), (1,1),(-1,1),(-1,-1),(1,-1)]
-- > [[(1.0,1.0),(-1.0,1.0),(-1.0,-1.0)],[(1.0,1.0),(-1.0,-1.0),(1.0,-1.0)]]
toTriangle :: [(Double, Double)] -> [[(Double, Double)]]
toTriangle [] = []
toTriangle (_:bounds) =
  triangulate bounds

-- | Triangulate given polygon and return a list of triangles.
-- It is assumed that the given polygon's points are given anticlockwise.
-- First and last point in the list don't need to be identical.  
-- usage example:
--
-- > triangulate [(1,1),(-1,1),(-1,-1),(1,-1)]
-- > [[(1.0,1.0),(-1.0,1.0),(-1.0,-1.0)],[(1.0,1.0),(-1.0,-1.0),(1.0,-1.0)]]
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
--   Triangle is assumed to be given clockwise.  
--   examples:
--
-- > pointInTriangle [(0,0), (0,1), (1,0)] (0.1,0.1) = True
-- > pointInTriangle [(0,0), (1,0), (0,1)] (0.1,0.1) = False
-- > pointInTriangle [(0,0), (0,1), (1,0)] (0,0) = False
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
--   E.g.  
--
-- > isAnyPointInTriangle [(0,0), (0,1), (1,0)] [(0.1,0.1), (0.2,0.2)] = True
isAnyPointInTriangle :: [(Double, Double)] -> [(Double, Double)] -> Bool
isAnyPointInTriangle triangle =
  any (pointInTriangle triangle)

-- | Check if given points are convex.
--   Points need to be given clockwise, e.g.
--
-- > isConvex (0,0) (0,1) (1,0) = False
-- > isConvex (0,0) (1,0) (0,1) = True
--
isConvex :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
isConvex (p1x, p1y) (px, py) (p2x, p2y)  =
  l < 0
  where
        l = (p1x - px) * (p2y - py) - (p1y - py) * (p2x - px)
