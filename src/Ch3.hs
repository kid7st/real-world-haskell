module Ch3 (len, mean, palindrome, isPalindrome, sortListOfList, joinsList,
            BTree(Empty, Node), heightOfBTree,
            Point, Direction(DLeft, DStraight, DRight), direction, directions,
            sortedPoints, graham) where

import Data.List

-- 1. “Write a function that computes the number of elements in a list. To test it, ensure that it gives the same answers as the standard length function.”
-- 2. “Add a type signature for your function to your source file. To test it, load the source file into ghci again.
len :: Foldable t => (t a) -> Int
len = foldr (\_ n -> 1 + n) 0

-- 3. Write a function that computes the mean of a list, i.e., the sum of all elements in the list divided by its length. (You may need to use the fromIntegral function to convert the length of the list from an integer into a floating-point number.)
numLen :: (Foldable t, Num a) => t a -> Int
numLen = len

numSum :: Foldable t => t Int -> Int
numSum = foldr (\x s -> x + s) 0

mean :: Foldable t => t Int -> Float
mean x = fromIntegral (numSum x) / fromIntegral (numLen x)

-- "4. Turn a list into a palindrome; i.e., it should read the same both backward and forward. For example, given the list [1,2,3], your function should return [1,2,3,3,2,1]."
palindrome :: [a] -> [a]
palindrome list = list ++ reverse list

-- "5. Write a function that determines whether its input list is a palindrome"
isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = (reverse list) == list

-- "6. Create a function that sorts a list of lists based on the length of each sublist. (You may want to look at the sortBy function from the Data.List module.)"
sortListOfList :: Foldable t => [t a] -> [t a]
sortListOfList = sortBy (\x -> (\y -> length x `compare` length y))

-- "7. Define a function that joins a list of lists together using a separator value"
joinsList :: a -> [[a]] -> [a]
joinsList sep [x] = x
joinsList sep (x:xs) = x ++ sep : (joinsList sep xs)
joinsList sep _ = []

-- "8. Using the binary tree type that we defined earlier in this chapter, write a function that will determine the height of the tree."

-- Define a binary tree
data BTree a = Empty | Node a (BTree a) (BTree a)
  deriving (Eq, Show)

heightOfBTree :: BTree a -> Int
heightOfBTree Empty = 0
heightOfBTree (Node _ left right) = 1 + max (heightOfBTree left) (heightOfBTree right)

-- "9. “Define a Direction data type that lets you represent these possibilities.”

type Point = (Double, Double)

data Direction = DLeft | DStraight | DRight
  deriving (Eq, Show)

-- "10. Write a function that calculates the turn made by three two-dimensional points and returns a Direction."

crossProduct :: Point -> Point -> Point -> Double
crossProduct (x1, y1) (x2, y2) (x3, y3) = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)

direction :: Point -> Point -> Point -> Direction
direction p1 p2 p3
  | cross < 0 = DLeft
  | cross > 0 = DRight
  | otherwise = DStraight
  where cross = crossProduct p1 p2 p3

-- "11. Define a function that takes a list of two-dimensional points and computes the direction of each successive triple."

directions :: [Point] -> [Direction]
directions (x:xs@(y:z:_)) = direction x y z : directions xs
directions _ = []

-- "12. Implement Graham’s scan algorithm for the convex hull of a set of 2D points.”

-- Get the min point and sort the points by x,y and direction
sortedPoints :: [Point] -> [Point]
sortedPoints points = minP : sortBy cmp (filter (\p -> p /= minP) points)
  where minP = getMinPoint points
          where  getMinPoint (p:ps) = foldl minPoint p ps
                   where minPoint (x1, y1) (x2, y2)
                           | y1 < y2 = (x1, y1)
                           | y1 > y2 = (x2, y2)
                           | x1 < x2 = (x1, y1)
                           | otherwise = (x2, y2)
        cmp p1 p2
          | ((direction minP p1 p2) == DLeft) = LT
          | ((direction minP p1 p2) == DRight) = GT
          | otherwise = if (dis minP p1) < (dis minP p2) then LT else GT
              where dis (x1, y1) (x2, y2) = sqrt ((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2))

graham :: [Point] -> [Point]
graham points = doGraham (sortedPoints points)
  where doGraham sortedPs = case sortedPs of
                              [p0,p1,p2] -> [p0, p1, p2]
                              p0:p1:p2:p3:ps -> if direction p1 p2 p3 == DRight
                                then doGraham (p0:p1:p3:ps)
                                else p0 : doGraham (p1:p2:p3:ps)