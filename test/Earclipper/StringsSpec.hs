module Earclipper.StringsSpec where

import Earclipper.Strings
import System.FilePath

import Test.Hspec

fixturePath = "test" </> "fixtures"

spec :: Spec
spec =
  describe "triangulateString" .
    it "should triangulate polygon" $ do
      polygon <- readFile $ fixturePath </> "polygon.txt"
      expected <- readFile $ fixturePath </> "result.txt"
      triangulateString polygon `shouldBe` expected
