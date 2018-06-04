module Ch4 (safeHead, safeTail, safeLast, safeInit, splitWith, asInt_fold, asInt_either, concat', takeWhile', groupBy, unlines') where

import Data.Char
import Data.List (foldl')

-- EXERCISES PART 1

-- 1. Write your own safe definitions of the standard partial list functions, but make sure they never fail.
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just $ init xs

-- 2. Write a function splitWith that acts similarly to words but takes a predicate and a list of any type, and then splits its input list on every element for which the predicate returns False
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith predicate xs = first : splitWith predicate (safeLast second)
  where
    (first, second) = span predicate xs
    safeLast [] = []
    safeLast (_:xs) = xs


-- EXERCISES PART 2

-- 1. Use a fold (choosing the appropriate fold will make your code much simpler) to rewrite and improve upon the asInt function from the earlier sectionExplicit Recursion.
-- 2. Extend your function to handle the following kinds of exceptional conditions by calling error
asInt_fold :: String -> Int
asInt_fold "" = 0
asInt_fold ('-':xs) = -1 * (asInt_fold xs)
asInt_fold str = foldl' step 0 str
  where
    step acc ch = acc * 10 + (digitToInt ch)

-- 3. The asInt_fold function uses error, so its callers cannot handle errors. Rewrite the function to fix this problem
type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
asInt_either "" = Left "Invalid input"
asInt_either ('-':xs) = case asInt_either xs of
                          Left error -> Left error
                          Right result -> Right (-1 * result)
asInt_either str = foldl' step (Right 0) str
  where
    step (Right result) ch
      | isDigit ch = Right (result * 10 + (digitToInt ch))
      | otherwise = Left ("non-digit '" ++ ch:'\'':[])
    step (Left error) ch = Left error

-- 4. The Prelude function concat concatenates a list of lists into a single list and has the following type:
   -- file: ch04/ch04.exercises.hs
   -- concat :: [[a]] -> [a]
concat' :: [[a]] -> [a]
concat' = foldr (++) []

-- 5. Write your own definition of the standard takeWhile function, first using explicit recursion, and then foldr.
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' predicate xs = foldr step [] xs
  where
    step x acc
      | predicate x = x : acc
      | otherwise = []

-- 6. The Data.List module defines a function, groupBy, which has the following type:
--  groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy predicate [] = []
groupBy predicate (x:xs) = (x:ys) : groupBy predicate zs
  where
    (ys, zs) = span (predicate x) xs

-- 7. How many of the following Prelude functions can you rewrite using list folds?
--  any， cycle, words, unlines
any :: (a -> Bool) -> [a] ->Bool
any predicate xs = foldr step False xs
  where
    step x acc
      | predicate x = True
      | otherwise = False || acc

cycle' :: [a] -> [a]
cycle' [] = error "cycle with Empty list"
cycle' xs = xs ++ (cycle xs)

words' :: String -> [String]
words' xs = word : words' rest
  where
    (word, rest) = span isSpace xs

unlines' :: [String] -> String
unlines' xs = foldr step "" xs
  where
    step x acc = x ++ "/n" ++ acc
