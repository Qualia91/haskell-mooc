-- Exercise set 4a:
--
-- * using type classes
-- * working with lists
--
-- Type classes you'll need
--  * Eq
--  * Ord
--  * Num
--  * Fractional
--
-- Useful functions:
--  * maximum
--  * minimum
--  * sort

module Set4a where

import Mooc.Todo
import Data.List
import Data.Ord
import qualified Data.Map as Map
import Data.Array

------------------------------------------------------------------------------
-- Ex 1: implement the function allEqual which returns True if all
-- values in the list are equal.
--
-- Examples:
--   allEqual [] ==> True
--   allEqual [1,2,3] ==> False
--   allEqual [1,1,1] ==> True
--
-- PS. check out the error message you get with your implementation if
-- you remove the Eq a => constraint from the type!


allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual [_] = True
allEqual (x:y:xs) = x == y && allEqual (y:xs)

-- answer way uses all function: checks predicate against list
-- allEqual :: Eq a => [a] -> Bool
-- allEqual [] = True
-- allEqual (x:xs) = all (==x) xs

------------------------------------------------------------------------------
-- Ex 2: implement the function distinct which returns True if all
-- values in a list are different.
--
-- Hint: a certain function from the lecture material can make this
-- really easy for you.
--
-- Examples:
--   distinct [] ==> True
--   distinct [1,1,2] ==> False
--   distinct [1,2] ==> True

instancesOfNumberInList :: Eq a => [a] -> Integer -> a -> Bool
instancesOfNumberInList [] ins y = ins <= 1
instancesOfNumberInList (x:xs) ins y
  | x == y = instancesOfNumberInList xs (ins + 1) y 
  | otherwise = instancesOfNumberInList xs ins y

distinct :: Eq a => [a] -> Bool
distinct [] = True
distinct xs = foldr (&&) True (map (instancesOfNumberInList xs 0) xs)

-- answer uses nub function: removes duplicates from a list
-- distinct :: Eq a => [a] -> Bool
-- distinct xs = xs == nub xs

--------------------------------------------------------------------------
-- Ex 3: implement the function middle that returns the middle value
-- (not the smallest or the largest) out of its three arguments.
--
-- The function should work on all types in the Ord class. Give it a
-- suitable type signature.
--
-- Examples:
--   middle 'b' 'a' 'c'  ==> 'b'
--   middle 1 7 3        ==> 3

middle :: Ord a => a -> a -> a -> a
middle a b c = sort [a, b, c] !! 1

------------------------------------------------------------------------------
-- Ex 4: return the range of an input list, that is, the difference
-- between the smallest and the largest element.
--
-- Your function should work on all suitable types, like Float and
-- Int. You'll need to add _class constraints_ to the type of range.
--
-- It's fine if your function doesn't work for empty inputs.
--
-- Examples:
--   rangeOf [4,2,1,3]          ==> 3
--   rangeOf [1.5,1.0,1.1,1.2]  ==> 0.5

rangeOf :: (Num a, Ord a) => [a] -> a
rangeOf xs =  (sorted !! (length sorted - 1)) - (sorted !! 0) 
  where sorted = sort xs

-- answer uses maximum and minimum: find max and min in a list
-- rangeOf :: (Num a, Ord a) => [a] -> a
-- rangeOf xs = maximum xs - minimum xs

------------------------------------------------------------------------------
-- Ex 5: given a list of lists, return the longest list. If there
-- are multiple lists of the same length, return the list that has
-- the smallest _first element_.
--
-- (If multiple lists have the same length and same first element,
-- you can return any one of them.)
--
-- Give the longest function a suitable type.
--
-- Examples:
--   longest [[1,2,3],[4,5],[6]] ==> [1,2,3]
--   longest ["bcd","def","ab"] ==> "bcd"

longest :: Ord a => [[a]] -> [a]
longest xss = sortByFirst (filter (\x -> length x == longestArray) sortedByLength) !! 0
  where { sortedByLength = sortByLength xss ;
  		  longestArray = length (last sortedByLength) }

sortByLength :: Ord a => [[a]] -> [[a]]
sortByLength = sortBy (comparing length)

sortByFirst :: Ord a => [[a]] -> [[a]]
sortByFirst = sortBy (comparing head)

-- answer is just better than my solution
-- longest :: Ord a => [[a]] -> [a]
-- longest = last . sortBy (comparing length) . reverse . sortBy (comparing head)

------------------------------------------------------------------------------
-- Ex 6: Implement the function incrementKey, that takes a list of
-- (key,value) pairs, and adds 1 to all the values that have the given key.
--
-- You'll need to add _class constraints_ to the type of incrementKey
-- to make the function work!
--
-- The function needs to be generic and handle all compatible types,
-- see the examples.
--
-- Examples:
--   incrementKey True [(True,1),(False,3),(True,4)] ==> [(True,2),(False,3),(True,5)]
--   incrementKey 'a' [('a',3.4)] ==> [('a',4.4)]

incrementKey :: (Eq k, Num v) => k -> [(k,v)] -> [(k,v)]
incrementKey k [] = []
incrementKey k (x:xs)
  | (fst x) == k = ((fst x), (snd x) + 1) : incrementKey k xs
  | otherwise = ((fst x), (snd x)) : incrementKey k xs

-- answer uses map
-- incrementKey :: (Eq k, Num v) => k -> [(k,v)] -> [(k,v)]
-- incrementKey k kvs = map incr kvs
--     where incr (k',v)
--               | k'==k = (k',v+1)
--               | otherwise = (k',v)

------------------------------------------------------------------------------
-- Ex 7: compute the average of a list of values of the Fractional
-- class.
--
-- There is no need to handle the empty list case.
--
-- Hint! since Fractional is a subclass of Num, you have all
-- arithmetic operations available
--
-- Hint! you can use the function fromIntegral to convert the list
-- length to a Fractional

average :: Fractional a => [a] -> a
average xs = (foldr (+) 0 xs) / (fromIntegral (length xs))

-- answer uses sum: foldr with a + as the function
-- average :: Fractional a => [a] -> a
-- average xs = sum xs / fromIntegral (length xs)

------------------------------------------------------------------------------
-- Ex 8: given a map from player name to score and two players, return
-- the name of the player with more points. If the players are tied,
-- return the name of the first player (that is, the name of the
-- player who comes first in the argument list, player1).
--
-- If a player doesn't exist in the map, you can assume they have 0 points.
--
-- Hint: Map.findWithDefault can make this simpler
--
-- Examples:
--   winner (Map.fromList [("Bob",3470),("Jane",2130),("Lisa",9448)]) "Jane" "Lisa"
--     ==> "Lisa"
--   winner (Map.fromList [("Mike",13607),("Bob",5899),("Lisa",5899)]) "Lisa" "Bob"
--     ==> "Lisa"

getFromMaybe :: Maybe Int -> Int
getFromMaybe x = case x of
  Just a -> a
  Nothing -> 0

winner :: Map.Map String Int -> String -> String -> String
winner scores player1 player2
  | player1Score > player2Score = player1
  | player1Score == player2Score = player1
  | otherwise = player2
    where { player1Score = getFromMaybe (Map.lookup player1 scores) ;
	        player2Score = getFromMaybe (Map.lookup player2 scores) }

-- should have just implimented a score function, i did the same thing twice in my where
-- winner :: Map.Map String Int -> String -> String -> String
-- winner scores player1 player2
--   | score player2 > score player1 = player2
--   | otherwise = player1
--   where score p = Map.findWithDefault 0 p scores

------------------------------------------------------------------------------
-- Ex 9: compute how many times each value in the list occurs. Return
-- the frequencies as a Map from value to Int.
--
-- Challenge 1: try using Map.alter for this
--
-- Challenge 2: use foldr to process the list
--
-- Example:
--   freqs [False,False,False,True]
--     ==> Map.fromList [(False,3),(True,1)]

-- Challenge 1
freqs :: (Eq a, Ord a) => [a] -> Map.Map a Int
freqs xs = freqs' xs Map.empty

freqs' :: (Eq a, Ord a) => [a] -> Map.Map a Int -> Map.Map a Int
freqs' [] map = map
freqs' (x:xs) map = freqs' xs (Map.alter f x map)
	where f _ = case (Map.lookup x map) of
		Just z -> Just (z + 1)
		Nothing -> Just 1

-- Answer challenge
-- freqs :: (Eq a, Ord a) => [a] -> Map.Map a Int
-- freqs = foldr (Map.alter $ Just . maybe 1 (+1)) Map.empty

------------------------------------------------------------------------------
-- Ex 10: recall the withdraw example from the course material. Write a
-- similar function, transfer, that transfers money from one account
-- to another.
--
-- However, the function should not perform the transfer if
-- * the from account doesn't exist,
-- * the to account doesn't exist,
-- * the sum is negative,
-- * or the from account doesn't have enough money.
--
-- Hint: there are many ways to implement this logic. Map.member or
-- Map.notMember might help.
--
-- Examples:
--   let bank = Map.fromList [("Bob",100),("Mike",50)]
--   transfer "Bob" "Mike" 20 bank
--     ==> fromList [("Bob",80),("Mike",70)]
--   transfer "Bob" "Mike" 120 bank
--     ==> fromList [("Bob",100),("Mike",50)]
--   transfer "Bob" "Lisa" 20 bank
--     ==> fromList [("Bob",100),("Mike",50)]
--   transfer "Lisa" "Mike" 20 bank
--     ==> fromList [("Bob",100),("Mike",50)]

transfer :: String -> String -> Int -> Map.Map String Int -> Map.Map String Int
transfer from to amount bank 
  | amount > 0 && (Map.member from bank) && (Map.member to bank) && (((Map.findWithDefault 0 from bank) - amount) >= 0) = Map.adjustWithKey plus_amount to (Map.adjustWithKey minus_amount from bank)
  | otherwise = bank
    where { plus_amount key x = x + amount ;
  		    minus_amount key x = x - amount }

------------------------------------------------------------------------------
-- Ex 11: given an Array and two indices, swap the elements in the indices.
--
-- Example:
--   swap 2 3 (array (1,4) [(1,"one"),(2,"two"),(3,"three"),(4,"four")])
--         ==> array (1,4) [(1,"one"),(2,"three"),(3,"two"),(4,"four")]

swap :: Ix i => i -> i -> Array i a -> Array i a
swap i j arr = (arr // first) // last
  where first = [(j, arr ! i)]
        last = [(i, arr ! j)]

-- could have put both replaces in one replace line
-- swap :: Ix i => i -> i -> Array i a -> Array i a
-- swap i j arr = arr // [(i,arr!j), (j, arr!i)]

------------------------------------------------------------------------------
-- Ex 12: given an Array, find the index of the largest element. You
-- can assume the Array isn't empty.
--
-- You may assume that the largest element is unique.
--
-- Hint: check out Data.Array.indices or Data.Array.assocs
-- (last element_list)
maxIndex :: (Ix i, Ord a) => Array i a -> i
maxIndex arr = maxIndex' (assocs arr) max_element 
  where max_element = last (sort (elems arr))

maxIndex' (x:associationList) maxElement
  | snd x == maxElement = fst x
  | otherwise = maxIndex' associationList maxElement

-- answer uses maximumBy function and a lambda
-- maxIndex :: (Ix i, Ord a) => Array i a -> i
-- maxIndex arr = index
--   where (index, _) = maximumBy (\(_,x) (_,y) -> compare x y) (assocs arr)