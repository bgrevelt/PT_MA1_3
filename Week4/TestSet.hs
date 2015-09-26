{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module TestSet

where 
  
import Lecture2
import SetOrd
import Test.QuickCheck
import SetOperations
import Data.List

{--
Implement a random data generator for the datatype Set Int, where Set is as defined in SetOrd.hs. 
First do this from scratch, next give a version that uses QuickCheck to random test this datatype.

Time spent: Approx 2 Hrs. (of which 1,5 where spent trying to get the QuickCheck stuff to work without flexblleInstances)
--}

genSetList :: IO (Set Int)
genSetList = do
  li <- genIntList
  return (list2set li)
  

instance Arbitrary (Set Int) where
  arbitrary = sized $ \s -> do
	  xs <- vectorOf (min s 100) (choose ( 1, 100))
	  return (list2set xs)


{--
  Testable properties
--}

-- General property for all set operation. We pass in the function as the 
-- first argument. The 2nd and 3rd arguments are two sets. The function is
-- then applied to these two sets and we check if the resulting set is 
-- properly formed (e.g sorted and unique)
nonDuplSet :: (Set Int -> Set Int -> Set Int) -> Set Int -> Set Int -> Bool
nonDuplSet f (Set a) (Set b) = let (Set y) = f (Set a) (Set b) in
	nub y == y
	
sortedSet :: (Set Int -> Set Int -> Set Int) -> Set Int -> Set Int -> Bool
sortedSet f (Set a) (Set b) = let (Set y) = f (Set a) (Set b) in
	sort y == y

{-- Testable properties for union --}
-- Checks if all elements in the output of the union function are elements
-- in either one of the input sets
unionAllOutputInInput :: Set Int -> Set Int -> Bool
unionAllOutputInInput (Set a) (Set b) = let (Set u) = setUnion (Set a) (Set b) in
	all (\x -> ((elem x a) || (elem x b))) u

-- Checks if all elements in the input sets are also elements in the output set
unionAllInputInOutput :: Set Int -> Set Int -> Bool
unionAllInputInOutput (Set a) (Set b) = let (Set u) = setUnion (Set a) (Set b) in
	(all (\x -> (x `elem` u)) a) && (all (\x -> (x `elem` u)) b)

{-- Testable properties for intersection --}
-- Checks if all elements in the set that's the output of the intersec 
-- function are elements in both input sets
intAllOutputInBothIn :: Set Int -> Set Int -> Bool
intAllOutputInBothIn (Set a) (Set b) = let (Set i) = setIntersect (Set a) (Set b) in
	all (\x -> ((x `elem` a) && (x `elem` b))) i

-- Checks if all elements that are present in both input sets of the 
-- intersect function are also elements in the output set of the intersect
-- function	
intBothInInOut :: Set Int -> Set Int -> Bool
intBothInInOut (Set a) (Set b) = let (Set i) = setIntersect (Set a) (Set b) in
	all (\x -> (not (x `elem` b)) || (x `elem` i)) a
	
-- Check if all elements in the output set of the intersect function are 
-- elements of set a and not of set b	
diffAllOutInOneIn :: Set Int -> Set Int -> Bool
diffAllOutInOneIn (Set a) (Set b) = let (Set d) = setDiff (Set a) (Set b) in
	all (\x -> ((x `elem` a) && not (x `elem` b))) d

-- Check if each element in set a that is not an element of set b is in the 
-- diff set
diffAllNotMutalInInOut :: Set Int -> Set Int -> Bool
diffAllNotMutalInInOut (Set a) (Set b) = let (Set d) = setDiff (Set a) (Set b) in
	(all (\x -> (x `elem` b) || (x `elem` d)) a) 


{-- function below is for debugging / informational purposes only. 
	We are fairly sure that this is not the right way to do this, 
	but it is convenient to have a function that runs all the tests
--}
testSetProperties :: IO()
testSetProperties = do 
	-- union
	a <- (quickCheckWith stdArgs { maxSuccess = 1000 } (nonDuplSet setUnion))
	b <- (quickCheckWith stdArgs { maxSuccess = 1000 } (sortedSet setUnion))
	c <- (quickCheckWith stdArgs { maxSuccess = 1000 } unionAllOutputInInput)
	d <- (quickCheckWith stdArgs { maxSuccess = 1000 } unionAllInputInOutput) 
	-- intersection
	e <- (quickCheckWith stdArgs { maxSuccess = 1000 } (nonDuplSet setIntersect))
	f <- (quickCheckWith stdArgs { maxSuccess = 1000 } (sortedSet setIntersect))
	g <- (quickCheckWith stdArgs { maxSuccess = 1000 } intAllOutputInBothIn)
	h <- (quickCheckWith stdArgs { maxSuccess = 1000 } intBothInInOut)
	-- difference
	i <- (quickCheckWith stdArgs { maxSuccess = 1000 } (nonDuplSet setDiff))
	j <- (quickCheckWith stdArgs { maxSuccess = 1000 } (sortedSet setDiff))
	l <- (quickCheckWith stdArgs { maxSuccess = 1000 } diffAllOutInOneIn)
	m <- (quickCheckWith stdArgs { maxSuccess = 1000 } diffAllNotMutalInInOut)
	return a
	
	
	
{-
Automated test of sets without quickCheck
-}	
message :: Bool -> IO()
message True = putStrLn "test passed"  
message False = putStrLn "test failed"  



myTestSet :: (Set Int -> Set Int -> Bool) -> IO ()
myTestSet function = do 
  a <- genSetList
  b <- genSetList
  c <- (message (function a b))
  return c

myTestSet2 :: (t -> Set Int -> Set Int -> Bool) -> t -> IO ()
myTestSet2 property function = do 
  a <- genSetList
  b <- genSetList
  c <- (message (property function a b))
  return c

test1 = myTestSet2 nonDuplSet setUnion
test2 = myTestSet2 sortedSet setUnion
test3 = myTestSet unionAllOutputInInput
test4 = myTestSet unionAllOutputInInput
test5 = myTestSet2 nonDuplSet setIntersect
test6 = myTestSet2 sortedSet setIntersect
test7 = myTestSet intAllOutputInBothIn 
test8 = myTestSet intBothInInOut
test9 = myTestSet2 nonDuplSet setDiff

test10 = myTestSet2 sortedSet setDiff
test11 = myTestSet intBothInInOut
test12 = myTestSet diffAllOutInOneIn 
test13 = myTestSet diffAllNotMutalInInOut 
