module Main where

import Earclipper.Strings

main :: IO ()
main = do
  input <-getContents
  putStr $ triangulateString input
