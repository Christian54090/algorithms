import Data.List (sortOn, intersperse)

mrTestInput = [(0,1),(3,5),(4,8),(10,12),(9,10)]
-- expecting [(0,1),(3.8),(9,12)]

mergeRanges :: Ord a => [(a,a)] -> [(a,a)]
mergeRanges [] = []
mergeRanges tuples = merged : mergeRanges rest
  where sorted = sortOn fst tuples
        m1@(lb,ub1) = head sorted
        (list,rest) = span (\(b,_) -> b <= ub1) sorted
        merged = checkUpperBound m1 list

checkUpperBound :: Ord a => (a,a) -> [(a,a)] -> (a,a)
checkUpperBound tup@(lb,ub1) tups
  | ub1 > ub2  = tup
  | otherwise  = (lb,ub2)
  where ub2 = (snd . last) tups

mlTestInput1 = [3,4,6,10,11,15]
mlTestInput2 = [1,5,8,12,14,19]

mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists [] [] = []
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists xs@(x:xs') ys@(y:ys')
  | x <= y    = x : mergeLists xs' ys
  | otherwise = y : mergeLists xs ys'

