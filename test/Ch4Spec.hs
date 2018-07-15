module Ch4Spec (main, spec) where

import Test.Hspec
import Control.Exception (evaluate)

import Ch4

main ::IO()
main = hspec spec

spec = do
  describe "Chapter 4 EXCERCISES PART 1" $ do
    describe "1. Write your own safe definitions of the standard partial list functions, but make sure they never fail." $ do
      it "safeHead should accept []" $
        safeHead ([] :: [Int]) `shouldBe` Nothing
      it "safeTail should accept []" $
        safeTail ([] :: [Int]) `shouldBe` Nothing
      it "safeLast should accept []" $
        safeLast ([] :: [Int]) `shouldBe` Nothing
      it "safeInit should accept []" $
        safeInit ([] :: [Int]) `shouldBe` Nothing

    describe "2. Write a function splitWith that acts similarly to words but takes a predicate and a list of any type, and then splits its input list on every element for which the predicate returns False" $ do
      it "should split a normal sentense" $
        splitWith (/= ' ') "This is a test" `shouldBe` ["This", "is", "a", "test"]
      it "should split a input 'This__is__a_test'" $
        splitWith (/= '_') "This__is__a_test" `shouldBe` ["This", "", "is", "", "a", "test"]

  describe "Chapter 4 EXCERCISES PART 2" $ do
    describe "1. and 2. Use a fold (choosing the appropriate fold will make your code much simpler) to rewrite and improve upon the asInt function from the earlier sectionExplicit Recursion." $ do
      it "should handle 101" $
        asIntFold "101" `shouldBe` 101
      it "should handle -31337" $
        asIntFold "-31337" `shouldBe` -31337
      it "should handle ''" $
        asIntFold "" `shouldBe` 0
      it "should handle -" $
        asIntFold "-" `shouldBe` 0
      it "should handle -3" $
        asIntFold "-3" `shouldBe` -3
      it "should not handle 2.7" $
        evaluate (asIntFold "2.7") `shouldThrow` anyException

    describe "3. The asInt_fold function uses error, so its callers cannot handle errors. Rewrite the function to fix this problem" $ do
      it "should handle 33" $
        asIntEither "33" `shouldBe` Right 33
      it "should return an error" $
        asIntEither "foo" `shouldBe` Left "non-digit 'f'"

    describe "4. The Prelude function concat concatenates a list of lists into a single list and has the following typ: concat :: [[a]] -> [a]" $ do
      it "should concat list of strings" $
        concat' ["This", " ", "is", " ", "a", " ", "test"] `shouldBe` "This is a test"
      it "should concat list of int list " $
        concat' [[1], [2, 3], [4, 5, 6], [7, 8, 9, 0]] `shouldBe` [1, 2, 3, 4, 5, 6, 7, 8, 9, 0]

    describe "5. Write your own definition of the standard takeWhile function, first using explicit recursion, and then foldr." $ do
      it "should take sub list from a finite list" $
        takeWhile' (<5) [1, 2, 3, 4, 5, 6, 7, 8] `shouldBe` [1, 2, 3, 4]
      it "should take sub list from an infinite list" $
        takeWhile' (<5) [1, 2..] `shouldBe` [1, 2, 3, 4]

    describe "6. The Data.List module defines a function, groupBy" $
      it "should group equal number together" $
      groupBy (==) [1, 1, 2, 3, 3, 1] `shouldBe` [[1, 1], [2], [3, 3], [1]]

    describe "7. How many of the following Prelude functions can you rewrite using list folds? any， cycle, words, unlines" $ do
      it "any returns True" $
        any (== 0) [1, 2, 3, 4, 0] `shouldBe` True
      it "any returns False" $
        any (== 0) [1, 2, 3, 4, 5] `shouldBe` False
      it "words" $
        words "This is a test" `shouldBe` ["This", "is", "a", "test"]
      it "words []" $
        words [] `shouldBe` []
      it "unlines" $
        unlines' ["line1", "line2", "line3"] `shouldBe` "line1/nline2/nline3/n"
