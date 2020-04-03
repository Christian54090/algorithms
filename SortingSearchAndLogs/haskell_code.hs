import Data.List (sort)

-- BINARY SEARCH --

bSearch :: (Eq a, Ord a) => [a] -> a -> Bool
bSearch [] _ = False
bSearch (x:[]) y = x == y
bSearch xs y
  | y > mid   = bSearch higher y
  | y < mid   = bSearch lower y
  | otherwise = True
  where len = length xs `div` 2
        mid = xs !! len
        (lower, higher) = splitAt len xs

-- FIND REPEAT, SPACE EDITION --

-- list of integers
--  range of 1..n
--  length == n + 1

listOfInts = [1,2,3,4,4,5,6,7,8,9]

-- O(n)
findDup :: (Ord a, Eq a) => [a] -> a
findDup (x:y:xs)
  | x == y    = x
  | otherwise = findDup (y:xs)

-- O(n + n*lgn) ~unsorted~
findDup' :: (Ord a, Eq a) => [a] -> a
findDup' xs = findInSorted $ sort xs
  where findInSorted :: (Ord a, Eq a) => [a] -> a
        findInSorted (x:y:xs)
          | x == y    = x
          | otherwise = findInSorted (y:xs)
