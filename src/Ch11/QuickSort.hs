module Ch11.QuickSort
    where

import Test.QuickCheck

ch11_run :: IO ()
ch11_run = do
  quickCheck (prop_idempotent :: [Integer] -> Bool)
  quickCheck (prop_minimum :: [Integer] -> Property)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
  where
    lhs = filter (<x) xs
    rhs = filter (>=x) xs

prop_idempotent :: Ord a => [a] -> Bool
prop_idempotent xs = qsort (qsort xs) == qsort xs

prop_minimum :: Ord a => [a] -> Property
prop_minimum xs = not (null xs) ==> head (qsort xs) == minimum xs
