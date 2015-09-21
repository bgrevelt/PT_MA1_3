{-# LANGUAGE FlexibleInstances #-}

module SetGenerator

where 
  
import Lecture2
import SetOrd
import Test.QuickCheck
import Control.Monad
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
  arbitrary = do
	  n <- choose (0, 10)
	  xs <- vectorOf n (choose ( 1, 100))
	  return (list2set xs)


{--
Implement operations for set intersection, set union and set difference, for the datatype Set defined in SetOrd.hs. 
Next, use automated testing to check that your implementation is correct. First use your own generator, next use QuickCheck.
  
Time spent: 1 Hr
--}
  
setIntesect :: (Ord a) => Set a -> Set a -> Set a
setIntesect (Set as) (Set []) = (Set [])
setIntesect (Set as) (Set (b:bs)) = if (elem b as) then insertSet b (setIntesect (Set as) (Set bs)) else (setIntesect (Set as) (Set bs))
  
setUnion :: (Ord a) => Set a -> Set a -> Set a
setUnion (Set as) (Set []) = (Set as)
setUnion (Set as) (Set (b:bs)) = setUnion (insertSet b (Set as)) (Set bs)

setDiff :: (Ord a) => Set a -> Set a -> Set a
setDiff (Set as) (Set bs) = add (notInSet (Set as) (Set bs)) (notInSet (Set bs) (Set as)) where
	add (Set as) (Set bs) =	(Set (sort (as ++ bs)))

notInSet :: (Ord a) => Set a -> Set a -> Set a
notInSet (Set as) (Set []) = (Set [])
notInSet (Set as) (Set (b:bs)) = if (elem b as) then (notInSet (Set as) (Set bs)) else insertSet b (notInSet (Set as) (Set bs))