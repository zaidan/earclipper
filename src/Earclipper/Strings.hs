{-| String conversion
-}
module Earclipper.Strings
  (
    triangulateString
  , toTriangleString
  ) where

import Earclipper.EarClipping

-- | Triangulates the given polygons from string.
-- Each polygon is delimited by newline.
-- Returns the result as newline delimited string.
triangulateString :: String -> String
triangulateString input =
  let inputLines = lines input
      triangles = map toTriangleString inputLines
  in unlines triangles

-- | Triangulates the given polygons from string.
-- Returns the result as string.
toTriangleString :: String -> String
toTriangleString bounds =
  show $ toTriangle (read bounds :: [(Double, Double)])
