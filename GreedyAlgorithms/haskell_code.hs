import qualified Data.Map as M
import Data.List

-- APPLE STOCKS --

stockPrices = [10,7,5,8,11,9] :: [Int]
stockPrices' = [11,10,9,8,7,5] :: [Int]

getMaxProfit :: [Int] -> Int
getMaxProfit prices = sell - buy
  where buy = minimum $ init prices
        (_,maxList) = span (>buy) prices
        sell = maximum maxList

getMaxProfit' :: [Int] -> Int
getMaxProfit' prices = sell - buy
  where buy = foldr lowerNum (maxBound::Int) $ init prices
        (_,maxList) = span (>buy) prices
        sell = foldr biggerNum 0 $ tail maxList

biggerNum :: (Ord a, Num a) => a -> a -> a
biggerNum acc i =
  case (i > acc) of
    True -> i
    False -> acc

lowerNum :: (Ord a, Num a) => a -> a -> a
lowerNum acc i =
  case (i < acc) of
    True -> i
    False -> acc

-- HIGHEST PRODUCT OF 3 --

-- PRODUCT OF ALL OTHER NUMBERS --

-- CAFE ORDER CHECKER --

takeOut = [1,3,5]
dineIn  = [2,4,6]
served  = [1,2,3,5,4,6]
