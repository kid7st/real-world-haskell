module Ch3Spec (main, spec) where

import Test.Hspec

import Ch3

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec =
  describe "Chapter 3" $ do
    it "1. length of []" $ len [] `shouldBe` 0
    it "1. length of non empty list" $
      let input = [1, 2, 3, 4, 5]
      in len input `shouldBe` length input

    it "3. mean of a non empty list" $
      let input = [1, 2, 3, 4, 5]
      in mean input `shouldBe` (fromIntegral (sum input) / fromIntegral (length input))

    it "4. Turn a list into a palindrome" $
      let input = [1, 2, 3, 4, 5]
      in palindrome input `shouldBe` [1, 2, 3, 4, 5, 5, 4, 3, 2, 1]

    it "5. Determine a palindrome is a palindrome" $
      let input = [1, 2, 3, 3, 2, 1]
      in isPalindrome input `shouldBe` True

    it "5. Determine a not-palindrome is not a palindrome" $
      let input = [1, 2, 3, 4, 5]
      in isPalindrome input `shouldBe` False

    it "6. Sort a list of lists" $
      let input = [[1, 2, 3], [1], [1, 2, 3, 4], [1, 2]]
          output = [[1], [1, 2], [1, 2, 3], [1, 2, 3, 4]]
      in sortListOfList input `shouldBe` output

    it "7. Joins a list of lists together using a spearate value" $
      let input = ["foo", "bar", "baz", "quux"]
          output = "foo,bar,baz,quux"
      in joinsList ',' input `shouldBe` output

    it "8 Height of binary tree" $
      let input = Node 1 Empty (Node 1 Empty (Node 1 Empty Empty))
      in heightOfBTree input `shouldBe` 3

    it "10. Direction of points" $
      direction (0.0, 0.0) (1.0, 1.0) (1.0, 2.0) `shouldBe` DRight

    it "11. Directions of point list" $
      let input = [(0.0, 0.0), (1.0, 1.0), (1.0, 2.0), (2.0, 1.0), (2.0, 0.0), (2.0, -1.0)]
      in directions input `shouldBe` [DRight, DLeft, DLeft, DStraight]

    it "Sort points by angle and distance" $
      let input = [(1.0, 1.0), (3.0, 1.0), (4.0, 3.0), (3.0, 4.0), (-1.0, 3.0), (2.0, 2.0), (2.0, 3.0), (4.0, 4.0)]
      in sortedPoints input `shouldBe` [(1.0,1.0),(-1.0,3.0),(2.0,3.0),(3.0,4.0),(2.0,2.0),(4.0,4.0),(4.0,3.0),(3.0,1.0)]

    it "12. Graham’s scan algorithm for the convex hull of a set of 2D points" $
      let input = [(1.0, 1.0), (3.0, 1.0), (4.0, 3.0), (3.0, 4.0), (-1.0, 3.0), (2.0, 2.0), (2.0, 3.0), (4.0, 4.0)]
      in graham input `shouldBe` [(1.0,1.0),(-1.0,3.0),(3.0,4.0),(4.0,4.0),(4.0,3.0),(3.0,1.0)]