-- Exercise set 5b: playing with binary trees

module Set5b where

import Mooc.Todo

-- The next exercises use the binary tree type defined like this:

data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- Ex 1: implement the function valAtRoot which returns the value at
-- the root (top-most node) of the tree. The return value is Maybe a
-- because the tree might be empty (i.e. just a Empty)

valAtRoot :: Tree a -> Maybe a
valAtRoot Empty = Nothing
valAtRoot (Node a _ _) = Just a 


------------------------------------------------------------------------------
-- Ex 2: compute the size of a tree, that is, the number of Node
-- constructors in it
--
-- Examples:
--   treeSize (Node 3 (Node 7 Empty Empty) Empty)  ==>  2
--   treeSize (Node 3 (Node 7 Empty Empty) (Node 1 Empty Empty))  ==>  3

treeSize :: Tree a -> Int
treeSize Empty = 0
treeSize (Node a b c) = 1 + (treeSize b) + (treeSize c)

------------------------------------------------------------------------------
-- Ex 3: get the largest value in a tree of positive Ints. The
-- largest value of an empty tree should be 0.
--
-- Examples:
--   treeMax Empty  ==>  0
--   treeMax (Node 3 (Node 5 Empty Empty) (Node 4 Empty Empty))  ==>  5

treeMax :: Tree Int -> Int
treeMax Empty = 0
treeMax tree = treeMax' tree 0

treeMax' :: Tree Int -> Int -> Int
treeMax' Empty biggest = biggest
treeMax' (Node a b c) biggest
  | a > biggest = biggestVal (treeMax' b a) (treeMax' c a)
  | otherwise = biggestVal (treeMax' b biggest) (treeMax' c biggest)
    where 
  biggestVal left right
    | left > right = left
    | otherwise = right

------------------------------------------------------------------------------
-- Ex 4: implement a function that checks if all tree values satisfy a
-- condition.
--
-- Examples:
--   allValues (>0) Empty  ==>  True
--   allValues (>0) (Node 1 Empty (Node 2 Empty Empty))  ==>  True
--   allValues (>0) (Node 1 Empty (Node 0 Empty Empty))  ==>  False

allValues :: (a -> Bool) -> Tree a -> Bool
allValues condition Empty = True
allValues condition (Node a b c) = (condition a) && (allValues condition b) && (allValues condition c)

------------------------------------------------------------------------------
-- Ex 5: implement map for trees.
--
-- Examples:
--
-- mapTree (+1) Empty  ==>  Empty
-- mapTree (+2) (Node 0 (Node 1 Empty Empty) (Node 2 Empty Empty))
--   ==> (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty))

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Empty = Empty
mapTree f (Node a b c) = Node (f a) (mapTree f b) (mapTree f c) 

------------------------------------------------------------------------------
-- Ex 6: given a value and a tree, build a new tree that is the same,
-- except all nodes that contain the value have been removed. Also
-- remove the subnodes of the removed nodes.
--
-- Examples:
--
--     1          1
--    / \   ==>    \
--   2   0          0
--
--  cull 2 (Node 1 (Node 2 Empty Empty) (Node 0 Empty Empty))
--     ==> (Node 1 Empty
--                 (Node 0 Empty Empty))
--
--      1           1
--     / \           \
--    2   0   ==>     0
--   / \
--  3   4
--
--  cull 2 (Node 1 (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty)) (Node 0 Empty Empty))
--     ==> (Node 1 Empty
--                 (Node 0 Empty Empty)
--
--    1              1
--   / \              \
--  0   3    ==>       3
--   \   \
--    2   0
--
--  cull 0 (Node 1 (Node 0 Empty (Node 2 Empty Empty)) (Node 3 Empty (Node 0 Empty Empty)))
--     ==> (Node 1 Empty
--                 (Node 3 Empty Empty))

cull :: Eq a => a -> Tree a -> Tree a
cull val Empty = Empty
cull val (Node a b c)
  | a == val = Empty
  | otherwise = Node a (cull val b) (cull val c) 

------------------------------------------------------------------------------
-- Ex 7: check if a tree is ordered. A tree is ordered if:
--  * all values to the left of the root are smaller than the root value
--  * all of the values to the right of the root are larger than the root value
--  * and the left and right subtrees are ordered.
--
-- Hint: allValues will help you here!
--
-- Examples:
--         1
--        / \   is ordered:
--       0   2
--   isOrdered (Node 1 (Node 0 Empty Empty) (Node 2 Empty Empty))   ==>   True
--
--         1
--        / \   is not ordered:
--       2   3
--   isOrdered (Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty))   ==>   False
--
--           2
--         /   \
--        1     3   is not ordered:
--         \
--          0
--   isOrdered (Node 2 (Node 1 Empty (Node 0 Empty Empty)) (Node 3 Empty Empty))   ==>   False
--
--           2
--         /   \
--        0     3   is ordered:
--         \
--          1
--   isOrdered (Node 2 (Node 0 Empty (Node 1 Empty Empty)) (Node 3 Empty Empty))   ==>   True

getVal :: Ord a => Tree a -> a
getVal (Node a b c) = a

isOrdered :: Ord a => Tree a -> Bool
isOrdered Empty = True
isOrdered (Node val Empty Empty) = True
isOrdered (Node val Empty right)
  | val <= (getVal right) = True && (isOrdered right)
  | otherwise = False
isOrdered (Node val left Empty) 
  | val >= (getVal left) = True && (isOrdered left)
  | otherwise = False
isOrdered (Node val left right)
  | val >= (getVal left) && val <= (getVal right) = True && (isOrdered left) && (isOrdered right)
  | otherwise = False


------------------------------------------------------------------------------
-- Ex 8: a path in a tree can be represented as a list of steps that
-- go either left or right.

data Step = StepL | StepR
  deriving (Show, Eq)

-- Define a function walk that takes a tree and a list of steps, and
-- returns the value at that point. Return Nothing if you fall of the
-- tree (i.e. hit a Empty).
--
-- Examples:
--   walk [] (Node 1 (Node 2 Empty Empty) Empty)       ==>  Just 1
--   walk [StepL] (Node 1 (Node 2 Empty Empty) Empty)  ==>  Just 2
--   walk [StepL,StepL] (Node 1 (Node 2 Empty Empty) Empty)  ==>  Nothing

walk :: [Step] -> Tree a -> Maybe a
walk _ Empty = Nothing
walk [] (Node val left right) = Just val
walk (StepL:steps) (Node val Empty right) = Nothing
walk (StepL:steps) (Node val left right) = (walk steps left)
walk (StepR:steps) (Node val left Empty) = Nothing
walk (StepR:steps) (Node val left right) = (walk steps right)

------------------------------------------------------------------------------
-- Ex 9: given a tree, a path and a value, set the value at the end of
-- the path to the given value. Since Haskell datastructures are
-- immutable, you'll need to build a new tree.
--
-- If the path falls off the tree, do nothing.
--
-- Examples:
--   set [] 1 (Node 0 Empty Empty)  ==>  (Node 1 Empty Empty)
--   set [StepL,StepL] 1 (Node 0 (Node 0 (Node 0 Empty Empty) (Node 0 Empty Empty))  (Node 0 Empty Empty))
--                  ==>  (Node 0 (Node 0 (Node 1 Empty Empty)
--                                       (Node 0 Empty Empty))
--                               (Node 0 Empty Empty))
--
--   set [StepL,StepR] 1 (Node 0 Empty Empty)  ==>  (Node 0 Empty Empty)

set :: [Step] -> a -> Tree a -> Tree a
set [] val Empty = Empty
set _ _ Empty = Empty
set [] val (Node _ left right) = Node val left right
set (StepL:xs) val (Node nodeVal Empty right) = Node nodeVal Empty right
set (StepL:xs) val (Node nodeVal left right) = Node nodeVal (set xs val left) right
set (StepR:xs) val (Node nodeVal left Empty) = Node nodeVal left Empty
set (StepR:xs) val (Node nodeVal left right) = Node nodeVal left (set xs val right)

------------------------------------------------------------------------------
-- Ex 10: given a value and a tree, return a path that goes from the
-- root to the value. If the value doesn't exist in the tree, return Nothing.
--
-- You may assume the value occurs in the tree at most once.
--
-- Examples:
--   search 1 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty))  ==>  Just [StepL]
--   search 1 (Node 2 (Node 4 Empty Empty) (Node 3 Empty Empty))  ==>  Nothing
--   search 1 (Node 2 (Node 3 (Node 4 Empty Empty) (Node 1 Empty Empty)) (Node 5 Empty Empty)) ==>  Just [StepL,StepR]

isJustCust :: Eq a => Maybe a -> Bool
isJustCust Nothing = False
isJustCust _ = True

search :: Eq a => a -> Tree a -> Maybe [Step]
search val Empty = Nothing
search val tree = search' val tree []

search' :: Eq a => a -> Tree a -> [Step] -> Maybe [Step]
search' val Empty running = Nothing
search' val (Node nodeVal Empty right) running
  | val == nodeVal = Just running
  | otherwise = search' val right (running ++ [StepR])
search' val (Node nodeVal left Empty) running
  | val == nodeVal = Just running
  | otherwise = search' val left (running ++ [StepL])
search' val (Node nodeVal left right) running
  | val == nodeVal = Just running
  | isJustCust searchLeft = searchLeft
  | isJustCust searchRight = searchRight
  | otherwise = Nothing
  where searchLeft = (search' val left (running ++ [StepL]))
        searchRight = (search' val right (running ++ [StepR]))
