module Spec (main) where

import Test.Hspec (describe, hspec, it, shouldBe)
import Prelude

-- https://hspec.github.io/#:~:text=main%20%3A%3A%20io%20()%20main%20%3D%20hspec%20%24%20do%20describe%20%22prelude.head%22%20%24%20do%20it%20%22returns%20the%20first%20element%20of%20a%20list%22%20%24%20do%20head%20%5B23%20..%5D%20%60shouldbe%60%20(23%20%3A%3A%20int)
main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)
