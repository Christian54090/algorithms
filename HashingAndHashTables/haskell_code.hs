import Data.List
import Data.Char (toUpper, toLower)
import qualified Data.Map as M
import qualified Data.Set as S

-- INFLIGHT ENTERTAINMENT --

movieLengths :: (Eq a, Num a) => a -> [a] -> Bool
movieLengths _ [] = False
movieLengths flightLength (x:xs) =
  case movieLengths' flightLength x xs of
    (Right _) -> True
    (Left _)  -> movieLengths flightLength xs

movieLengths' :: (Eq a, Num a) => a -> a -> [a] -> Either [a] Bool
movieLengths' _ _ [] = Left []
movieLengths' flightLength x (y:ys)
  | x + y == flightLength = Right True
  | otherwise             = movieLengths' flightLength x ys

canFitTwoMovies :: (Eq a, Num a) => a -> [a] -> Bool
canFitTwoMovies _ [] = False
canFitTwoMovies fLen (x:xs)
  | any (\y -> y == mLen) xs = True
  | otherwise                = canFitTwoMovies fLen xs
  where mLen = fLen - x

-- PERMUTATION PALINDROME --

permutationIsPalindrome :: Eq a => [a] -> Bool
permutationIsPalindrome xs = foldr (\x y -> isPalindrome x || y) False $ permutations xs
  where isPalindrome :: Eq a => [a] -> Bool
        isPalindrome ys = ys == reverse ys

-- WORD CLOUD DATA --

wordCloud :: String -> M.Map String Int
wordCloud xs = M.fromListWith (+) (wordsToFromList xs)

wordsToFromList :: String -> [(String, Int)]
wordsToFromList xs = zip (words . filterToLower $ xs) [1,1..]

filterToLower :: String -> String
filterToLower [] = []
filterToLower (x:xs)
  | x `elem` letters = toLower x : filterToLower xs
  | otherwise        = ' ' : filterToLower xs
  where letters = ['a'..'z'] ++ ['A'..'Z'] ++ " -'"

testStr =
  "After beating the eggs, Dana read the next step: " ++
  "Add milk and eggs, then-add flour and sugar."

testStr2 =
  "We came, we saw, we conquered...then we ate Bill's (Mille-Feuille) cake." ++
  "The Bill came out to be five dollars."

-- TOP SCORES --

testUnsorted = [37,89,41,65,91,53]
highestPossible = 100
sortMap = M.empty

sortScores :: [Integer] -> [Integer]
sortScores scores = M.keys $ sortScores' scores M.empty

sortScores' :: [Integer] -> M.Map Integer Integer -> M.Map Integer Integer
sortScores' [] rMap     = rMap
sortScores' (x:xs) rMap = M.insertWith (+) x 1 $ sortScores' xs rMap