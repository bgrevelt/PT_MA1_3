{-# OPTIONS_GHC -XFlexibleInstances #-}
module Prep

where

-- -XFlexibleInstances

import Data.List
import System.Random
import System.IO.Unsafe
import Test.QuickCheck
import Control.Monad

--
-- Sets as defined by the capitals
-- 
naturals0 :: [Integer]
naturals0 = [0..]

naturals :: [Integer]
naturals = [1..]

negatives0 :: [Integer]
negatives0 = [0, -1..]

negatives :: [Integer]
negatives = [-1, -2..]

--
-- Common sequences
--
triangles :: [Integer]
triangles = scanl (+) 1 [2..]

factorials :: [Integer]
factorials = scanl (*) 1 [2..]

squares :: [Integer]
squares = [ x^2 | x <- naturals]

cubes :: [Integer]
cubes = [ x^3 | x <- naturals]

powers :: Integer -> [Integer]
powers n = [x^n | x <- naturals]

primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p : xs) = p : sieve [x | x <- xs, rem x p /= 0]

--
-- N'th element of sequence
--
triangle :: Integer -> Integer
triangle n = triangles !! fromIntegral n

factorial :: Integer -> Integer
factorial n = factorials !! fromIntegral n

square :: Integer -> Integer
square n = squares !! fromIntegral n

cube :: Integer -> Integer
cube n = cubes !! fromIntegral n

prime :: Integer -> Integer
prime n = primes !! fromIntegral n

--
-- Generate a random number
--
unsafeRand :: Int
unsafeRand = unsafePerformIO(getStdRandom(randomR(0, 9)))

randomInt :: Int -> IO Int
randomInt n = getStdRandom(randomR(0, n))

--
-- Random utility function to make it negative 50% of the time
--
randomFlip :: Int -> IO Int
randomFlip x = do 
   b <- randomInt 1
   return (if b == 0 then x else (-x))

--
-- Generate a random list
--
--
-- You can generate random lists with these functions          example
-- genIntList upperBound length                                [0, -3, 4, 4], [], [-2]
-- genRandomIntList upperBound length                          [0, -3, 4, 4], [], [-2]    More random and faster
-- genPositiveIntList upperBound length                        [2, 3, 0, 1], [], [2]
-- genPositiveRandomIntList upperBound length                  [2, 3, 0, 1], [], [2]      More random and faster
--

genIntList :: Int -> Int -> IO [Int] -- Take care, although the teachers made this this is in no way a random list
genIntList a b = do                  -- For low ranges you can make favorable predictions because the distribution is not uniform
   k <- randomInt a                  -- Param 1: range in which the values are generated
   n <- randomInt b                  -- Param 2: max size of the list
   getIntL k n                       -- Generate a random list with random values biased towards lower

genRandomIntList :: Int -> Int -> IO [Int] -- Uniform random list
genRandomIntList a b = do
   n <- randomInt b
   getIntL a n

genPositiveIntList :: Int -> Int -> IO [Int] -- Same as genIntList but returns a positive list
genPositiveIntList a b = do
   k <- randomInt a
   n <- randomInt b
   getPositiveIntL k n

genPositiveRandomIntList :: Int -> Int -> IO [Int] -- Uniform random list same: genRandomIntList
genPositiveRandomIntList a b = do
   n <- randomInt b
   getPositiveIntL a n

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []     -- Base case if size 0 \todo: negative integers xD
getIntL k n = do
   x <-  randomInt k        -- Take a random integer between 0 and Param 1
   y <- randomFlip x        -- add a 50% chance to get a negative number
   xs <- getIntL k (n - 1)  -- Get the next number (depends on the size of the Param 2)
   return (y : xs)          -- return y + the rest of the list

getPositiveIntL :: Int -> Int -> IO [Int] -- Does NOT add negatives
getPositiveIntL _ 0 = return []
getPositiveIntL k n = do
   x <-  randomInt k
   xs <- getPositiveIntL k (n - 1)
   return (x : xs)

--
-- Run test suite on something
--
-- runListSuite 0 100 isX
runListSuite :: Int -> Int -> ([Int] -> Bool) -> IO() -- Single list IN
runListSuite fwdIter lstIter func = if fwdIter == lstIter then print(show lstIter ++ " Tests passed")
  else do
    randTop <- randomInt 10
    param1 <- genIntList 10 randTop
    if func param1 then
       do print("Pass on: | " ++  show param1 ++ " |")
          runListSuite(fwdIter + 1) lstIter func
    else error(" Failed test on input list #1: " ++ show param1)

runIntSuite :: Int -> Int -> (Int -> Bool) -> IO() -- Single list IN
runIntSuite fwdIter lstIter func = if fwdIter == lstIter then print(show lstIter ++ " Tests passed")
  else do
    param1 <- randomInt 10
    if func param1 then
       do print("Pass on: | " ++  show param1 ++ " |")
          runIntSuite(fwdIter + 1) lstIter func
    else error(" Failed test on input list #1: " ++ show param1)

-- Kinda like checking for transitive
--testR :: Int -> Int -> ([Int] -> [Int])
--                    -> ([Int] -> [Int] -> Bool) -> IO ()
--testR k n f r = if k == n then print (show n ++ " tests passed")
--                else do
--                  xs <- genIntList
--                  if r xs (f xs) then
--                    do print ("pass on: " ++ show xs)
--                       testR (k+1) n f r
--                  else error ("failed test on: " ++ show xs)

--
-- Test functions
--
samelength :: [Int] -> [Int] -> Bool
samelength xs ys = length xs == length ys

--
-- Quickcheck
-- quickCheck func
prop_reverse :: [Int] -> [Int] -> Bool
prop_reverse xs ys = reverse (xs++ys) == reverse ys ++ reverse xs

propStringReverse :: [String] -> Bool
propStringReverse xs = reverse(reverse xs) == xs

--
-- Higher order functions
--
-- zipWith takes 2 elements (1 from both sets) and apply the function to it
-- zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
-- Example: zipWith max [1, 2, 3, 4] [4, 3, 2, 1]
-- Result : [4, 3, 3, 4]

-- flip :: (a -> b -> c) -> b -> a -> c 
-- Example: flip zip [1,2,3,4,5] "hello"
-- Result: [('h',1),('e',2),('l',3),('l',4),('o',5)] 
-- Example: zipWith (flip div) [2,2..] [10,8,6,4,2]
-- Result: [5,4,3,2,1]

-- map takes a function and a list and applies that function to every element in the list, producing a new list.
-- map is actually: [x + 3 | x <- [1,2,3]]
-- map :: (a -> b) -> [a] -> [b]
-- Example: map (+3) [1,2,3]
-- Result: [4, 5, 6]

-- filter is a function that takes a predicate and apples it to all elements
-- filter :: (a -> Bool) -> [a] -> [a]
-- Example: filter (>3) [1, 2, 3, 4, 5]
-- Result: [4, 5]
-- Example: filter (`elem` ['A'..'Z']) "i lauGh At You BecAuse u r aLL the Same"  
-- Result: "GAYBALLS"

--
-- Nice random example
-- largestDivisible :: (Integral a) => a  
-- largestDivisible = head (filter p [100000,99999..])  
--    where p x = x `mod` 3829 == 0 
--

-- takeWhile
-- This one stops after meeting the condition while filter keeps going
-- Example: takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]

-- foldl
-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- Example: foldl (+) 0 [1,2,3]
-- Result: 6
-- foldr uses : instead of ++

-- foldl1 and foldr1
-- They assume the first (or last) element of the list to be the starting value and then start the fold with the element next to it.
-- sum = fold1 (+)

-- scanl and scanr are like foldl and foldr, only they report all the intermediate accumulator states in the form of a list. 
-- scanl (+) 0 [3,5,2,1]  
-- [0,3,8,10,11]

-- oddSquareSum :: Integer  
-- oddSquareSum =   
--     let oddSquares = filter odd $ map (^2) [1..]  
--         belowLimit = takeWhile (<10000) oddSquares  
--     in  sum belowLimit

--
-- To infinite list
-- cycle "test " gives "test test test test test "etc.


--
-- Question 1
--

data Btree a = Leaf | B a (Btree a) (Btree a) deriving (Eq,Show)

mapT :: (a -> b) -> Btree a -> Btree b
mapT func (B typ left right) = B (func typ) (mapT func left) (mapT func right)
mapT func _ = Leaf

-- Test it
testTree1 = Leaf
testTree2 = B 10 Leaf Leaf
testTree3 = B (-10) (B (-15) Leaf Leaf) (B 10 (B 12 Leaf Leaf) Leaf)

testQuestion1 :: Show a => Num a => [Btree a] -> IO ()
testQuestion1 [] = print "Done"
testQuestion1 (x : xs) = do
  print $ mapT (+3) x
  testQuestion1 xs

--
-- Question 2
--

--  collects the items found by in-order traversal of a binary tree in a list. Next define a second
-- function for in-order traversal in right to left direction:
inOrder :: Btree a -> [a]
inOrder (Leaf) = []
inOrder (B typ left right) = inOrder left ++ [typ] ++ inOrder right

inOrderRev :: Btree a -> [a]
inOrderRev t = reverse $ inOrder t 

-- define a property that can be used to test the two functions by relating them to each other.
treeProperty :: Eq a => Btree a -> Bool
treeProperty t = inOrder t == reverse (inOrderRev t)

--
-- Question 3
--
makeT :: Ord a => [a] -> Btree a
makeT = foldr insertT Leaf

insertT :: Ord a => a -> Btree a -> Btree a
insertT x Leaf = B x Leaf Leaf
insertT x (B y left right)
  | x < y = B y (insertT x left) right
  | otherwise = B y left (insertT x right)

-- Define a sorting procedure for lists by means of in-order traversal of a tree created with makeT
sortList :: Ord a => [a] ->[a]
sortList xs = inOrder $ makeT xs

-- Test it


--
-- Question 4
--
-- data Btree a = Leaf | B a (Btree a) (Btree a) deriving (Eq,Show)
type Dict = Btree (String, String)

lemma, info :: (String, String) -> String
lemma (x,_) = x
info (_,y) = y

stringTree1 = Leaf
stringTree2 = B ("test", "desc") Leaf Leaf
stringTree3 = B ("c", "descC") (B ("b", "descB") Leaf Leaf) (B ("e", "descE") (B ("d", "descD") Leaf Leaf) Leaf)

lookUp :: String -> Dict -> [String] 
lookUp key (Leaf) = []
lookUp key (B typ left right)
  | fst typ == key   = [snd typ] 
  | fst typ > key    = lookUp key left
  | otherwise        = lookUp key right

-- Use inductive reasoning to show that your solution is correct.

--
-- Question 5
--
-- Write code for inserting a new item at the correct position in an ordered dictionary.
insertLemma :: (String, String) -> Dict -> Dict
insertLemma (lem, inf) (Leaf) = B (lem, inf) Leaf Leaf
insertLemma (lem, inf) (B typ left right)
  | fst typ == lem   = error "Lemma already exists"
  | fst typ > lem    = B typ (insertLemma (lem, inf) left) right 
  | otherwise        = B typ left (insertLemma (lem, inf) right)

-- write an automated test procedure for checking whether an ordered dictionary is still ordered after the insertion.
-- ordered :: Dict -> Bool

-- You can use the following invariant wrapper
--
--invar :: (a -> Bool) -> (a -> a) -> a -> a
--invar p = assert (\ x y -> not (p x) || p y)
--
--assert :: (a -> b -> Bool) -> (a -> b) -> a -> b
--assert p f x = let x’ = f x in
--  if p x x’ then x’ else error "assert"

--
-- Question 6
--



--
-- Example Quick Check
--

{--
http://www.cse.chalmers.se/~rjmh/QuickCheck/manual_body.html#16
data Tree = Leaf Int | Branch Tree Tree


then a generator for trees might be defined by
tree = oneof [liftM Leaf arbitrary,
	      liftM2 Branch tree tree]

However, there is always a risk that a recursive generator like this may fail to terminate, or produce very large results. 
To avoid this, recursive generators should always use the size control mechanism. For example,

tree = sized tree'
tree' 0 = liftM Leaf arbitrary
tree' n | n>0 = 
	oneof [liftM Leaf arbitrary,
	       liftM2 Branch subtree subtree]
  where subtree = tree' (n `div` 2)

--}

--data Btree a = Leaf | B a (Btree a) (Btree a) deriving (Eq,Show)

--tree = oneof [liftM Leaf,
--	      liftM2 B arbitrary (tree arbitrary) (tree arbitrary)]



instance Arbitrary a => Arbitrary (Btree a) where
     arbitrary = sized gtree
 
gtree 0 = return Leaf
gtree n = do
    x <- arbitrary 
    t1 <- subtree
    t2 <- subtree
    return (B x t1 t2)
  where subtree = gtree (n `div` 2)

{--

instance Arbitrary a =>  Arbitrary (Btree a) where
  arbitrary = sized tree'
    where
      tree' 0 = return Leaf
      tree' n | n > 0 = oneof [return Leaf, (liftM3 B arbitrary (subtree) (subtree))]
        where subtree = tree' (n `div` 2)

testFunc :: Btree a -> Bool
testFunc n = True

quickCheck testFunc
--}
