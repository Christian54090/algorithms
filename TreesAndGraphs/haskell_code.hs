import Data.Monoid

-- BINARY SEARCH TREE --

data Tree a = Empty
            | Node (Tree a) a (Tree a)
            deriving Show

printTree :: Show a => Tree a -> IO ()
printTree (Node l v r) =
  putStrLn $
    "Node (" ++
    show l ++ ")\n     " ++
    show v ++ "\n     (" ++
    show r ++ ")"

insert :: Ord a => a -> Tree a -> Tree a
insert a Empty = Node Empty a Empty
insert a (Node l v r)
  | v > a     = Node (insert a l) v r
  | otherwise = Node l v (insert a r)

append :: Ord a => Tree a -> Tree a -> Tree a
append Empty t2 = t2
append t1 Empty = t1
append t1@(Node l v r) t2@(Node _ v' _)
  | v > v'    = Node (append l t2) v r
  | otherwise = Node l v (append r t2)

tree1 :: Tree Int
tree1 =
  Node (Node Empty 1 Empty)
       2
       (Node Empty 3 Empty)

tree2 :: Tree Int
tree2 =
  Node (Node Empty 4 Empty)
       5
       (Node Empty 6 Empty)

tree3 :: Tree Int
tree3 =
  Node (Node Empty 7 Empty)
       8
       (Node Empty 9 Empty)

tree4 = (tree2 <> tree1) <> tree3

instance Ord a => Monoid (Tree a) where
  mempty = Empty
  mappend Empty t2 = t2
  mappend t1 Empty = t1
  mappend t1@(Node l v r) t2@(Node _ v' _)
    | v > v'    = Node (append l t2) v r
    | otherwise = Node l v (append r t2)

dfs :: (Ord a, Eq a) => Tree a -> a -> Bool
dfs Empty _ = False
dfs (Node l v r) a
  | v == a = True
  | not (dfs l a) = dfs r a
  | otherwise = True

-- 2ND LARGEST IN BST --

sndLargest :: Tree a -> a
sndLargest (Node _ v r) =
  case r of
    (Node _ _ Empty) -> v
    _                -> sndLargest r

-- FIND A DUPLICATE --

list1 = [3,4,2,3,1,5] :: [Int]
list2 = [3,1,2,2]     :: [Int]
list3 = [4,3,1,1,4]   :: [Int]

findDup :: [Int] -> Int
findDup xs@(x:xs') = duplicate
  where len = length xs - 1
        position = findLoop xs len $ len + 1
        steps = findLength xs position (xs !! (position-1)) 0
        -- find first node of the cycle
        -- (1) head of position n+1 as many steps as the cycle's length
        pointerAhead = findLoop xs steps $ len + 1
        -- (2) at position n+1, advance until pointers match
        pointerStart = len + 1
        duplicate = matchPointers xs pointerStart pointerAhead

-- start at position n+1 and walk n+1 steps to find a position guaranteed to be in a cycle
findLoop :: [Int] -> Int -> Int -> Int
findLoop xs 0 pos = pos
findLoop xs n pos = findLoop xs (n-1) $ xs !! (pos-1)

-- find length of the cycle by remembering a position in the cycle and counting
-- the steps it takes to get back to that position
findLength :: [Int] -> Int -> Int -> Int -> Int
findLength xs remembered current steps
  | current == remembered = steps
  | otherwise             = findLength xs remembered (xs !! (current-1)) (steps+1)

-- advance through cycle until pointers match
matchPointers :: [Int] -> Int -> Int -> Int
matchPointers xs start ahead
  | start == ahead = start
  | otherwise      = matchPointers xs (xs !! (start-1)) (xs !! (ahead-1))
