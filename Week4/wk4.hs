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
  arbitrary = sized $ \s -> do
	  xs <- vectorOf (min s 100) (choose ( 1, 100))
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

-- propUnion tests if the result of the setUnion functions is a union
-- According to the isUnion property
propUnion :: Set Int -> Set Int -> Bool
propUnion (Set a) (Set b) = isUnion (Set a) (Set b) (setUnion (Set a) (Set b))

-- Is union is a wrapper that tests two things
--- Are all the elements of first the two sets also elements of the third set
--- Are all elements in the third set in either the first or the second set
isUnion :: Ord a => Set a -> Set a -> Set a -> Bool
isUnion (Set a) (Set b) (Set y) = allIn (Set a) (Set b) (Set y) && consistOf (Set a) (Set b) (Set y)

consistOf :: Ord a => Set a -> Set a -> Set a -> Bool
consistOf (Set y) (Set a) (Set b) = (remove a (remove b y)) == [] where
	remove [] ys = ys
	remove (a:as) ys = remove as (delete a ys) 

-- Other implementation for consistOf seems to be more readable	
consistOf' :: Ord a => Set a -> Set a -> Set a -> Bool
consistOf' (Set []) _ _ = True
consistOf' (Set (y:ys)) (Set a) (Set b)	 = (elem y a || elem y b) && consistOf (Set ys) (Set a) (Set b)

allIn :: Ord a => Set a -> Set a -> Set a -> Bool
allIn (Set a) (Set b) (Set y) = (allIn' a y) && (allIn' b y) where
	allIn' [] _ = True
	allIn' (a:as) ys = elem a ys && allIn' as ys
	
isProperSet :: Set Int -> Bool
isProperSet (Set s) = list2set s == (Set s)