{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module TestSet

where 
  
import Lecture2
import SetOrd
import Test.QuickCheck
import SetOperations

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
isProperSet :: (Set Int -> Set Int -> Set Int) -> Set Int -> Set Int -> Bool
isProperSet f (Set a) (Set b) = let (Set y) = f (Set a) (Set b) in
	list2set y == (Set y)

{-- Testable properties for union --}
-- Checks if all elements in the output of the union function are elements
-- in either one of the input sets
unionAllOutputInInput :: Set Int -> Set Int -> Bool
unionAllOutputInInput (Set a) (Set b) = let (Set u) = setUnion (Set a) (Set b) in
	all (\x -> ((elem x a) || (elem x b))) u

-- Checks if all elements in the input sets are also elements in the output set
unionAllInputInOutput :: Set Int -> Set Int -> Bool
unionAllInputInOutput (Set a) (Set b) = let (Set u) = setUnion (Set a) (Set b) in
	(all (\x -> (elem x u)) a) && (all (\x -> (elem x u)) b)

{-- Testable properties for intersection --}
-- Checks if all elements in the set that's the output of the intersec 
-- function are elements in both input sets
intAllOutputInBothIn :: Set Int -> Set Int -> Bool
intAllOutputInBothIn (Set a) (Set b) = let (Set i) = setIntersect (Set a) (Set b) in
	all (\x -> ((elem x a) && (elem x b))) i

-- Checks if all elements that are present in both input sets of the 
-- intersect function are also elements in the output set of the intersect
-- function	
intBothInInOut :: Set Int -> Set Int -> Bool
intBothInInOut (Set a) (Set b) = let (Set i) = setIntersect (Set a) (Set b) in
	all (\x -> (not (elem x b)) || (elem x i)) a
	
-- Check if all elements in the output set of the intersect function are 
-- elements in one (not both!) of the input sets	
diffAllOutInOneIn :: Set Int -> Set Int -> Bool
diffAllOutInOneIn (Set a) (Set b) = let (Set d) = setDiff (Set a) (Set b) in
	all (\x -> ((elem x a) && not (elem x b)) || ((elem x b) && not (elem x a))) d

-- Chec if for each input set, each element that is not present in the other 
-- input set is present in the output set (e.g each element in each input set
-- is either present in the other input set, or the output set)	
diffAllNotMutalInInOut :: Set Int -> Set Int -> Bool
diffAllNotMutalInInOut (Set a) (Set b) = let (Set d) = setDiff (Set a) (Set b) in
	(all (\x -> (elem x b) || (elem x d)) a) && (all (\x -> (elem x a) || (elem x d)) b)



test :: IO()
test = do 
	-- union
	a <- (quickCheck (isProperSet setUnion))
	b <- (quickCheck unionAllOutputInInput)
	c <- (quickCheck unionAllInputInOutput) 
	-- intersection
	c <- (quickCheck (isProperSet setIntersect))
	d <- (quickCheck intAllOutputInBothIn)
	e <- (quickCheck intBothInInOut)
	-- difference
	f <- (quickCheck (isProperSet setDiff))
	g <- (quickCheck diffAllOutInOneIn)
	h <- (quickCheck diffAllNotMutalInInOut)
	return a
