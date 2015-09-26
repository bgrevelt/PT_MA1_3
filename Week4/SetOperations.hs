{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module SetOperations

where 
  
import SetOrd
import Data.List


-- Intersect by filtering out all elements in lhs which are present in rhs  
setIntersect :: (Ord a) => Set a -> Set a -> Set a
setIntersect (Set as) (Set bs) = list2set $ filter ((flip elem) as) bs

-- The 'why reinvent the wheel' solution...
setIntersect' :: (Ord a) => Set a -> Set a -> Set a
setIntersect' (Set as) (Set bs) = list2set $ intersect as bs
  
-- Create a union by simply adding the lists. Removing duplicates and sorting
-- is handled by the list2set function
setUnion :: (Ord a) => Set a -> Set a -> Set a
setUnion (Set as) (Set bs) = list2set (as ++ bs)

-- Diff: Remove all b from a.
-- Removing duplicates and sorting is handled by the list2set function
setDiff :: (Ord a) => Set a -> Set a -> Set a
setDiff (Set as) (Set bs) = list2set $ as \\ bs
