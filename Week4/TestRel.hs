{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module TestRel

where 
  
import Closures
import Data.List
import Test.QuickCheck
 
{----------- Some helper functions -------------}
-- transR: Determine if a relation is transitive. 
-- Probably stolen from the source code accompanying the
-- Haskell book (small chance that I've written it myself
-- as one of the exercises in the book)
transR :: Ord a => Rel a -> Bool
transR [] = True
transR s = and [ trans pair s | pair <- s ] where 
      trans (x,y) r = 
       and [ elem (x,v) r | (u,v) <- r, u == y ] 

-- SubRel: Determine if the lhs relation is a subset of the rhs relation
subRel :: Rel Int -> Rel Int -> Bool
subRel [] _ = True
subRel (s:sub) sup = elem s sup && subRel sub sup

-- Turn a list of n elements into a list of all sublists of size n-1
dropOneSubLists :: [a] -> [[a]]
dropOneSubLists xs = dropOneSubLists' xs ((length xs)-1) where
	dropOneSubLists' [] _ = []
	dropOneSubLists' _ 0 = []
	dropOneSubLists' xs n = (removeElemFromList n xs) : (dropOneSubLists' xs (n-1)) where
		removeElemFromList ::Int -> [a] -> [a]
		removeElemFromList n xs = let (a,b) = splitAt n xs in a ++ (tail b)

{-- symmetric closure properties for quickcheck --}

-- Set X must be a subset of the symmetric closure of set X
symSubSet :: Rel Int -> Bool
symSubSet r = let rel = sort $ nub r in	-- This is a bit ugly. I could not figure out 
								-- how to write a generator for relations and
								-- the standard one includes non-unique elements
	subRel rel (symClos rel)

-- The symmetric closure of set X must be symmetric	
symMetric :: Rel Int -> Bool
symMetric r = let sc = symClos (sort $ nub r) in
	all (\(x,y) -> (elem (y,x) sc)) sc

-- The symmetric closure of set X must be the smallest possilbe set for 
-- which the previous two properties hold. We check this by making sure
-- that for each element (x,y) in the SC, either (x,y) or (y,x) is part of X	
symSmallest :: Rel Int -> Bool
symSmallest r = let rel = sort $ nub r in
	all (\(x,y) -> ((elem (y,x) r) || (elem (x,y) r))) (symClos rel)


{-- Transitive closure properties for quickcheck --}

-- Set X must be a subset of the transitive closure of set X
transSubSet :: Rel Int -> Bool
transSubSet r = let rel = sort $ nub r in
	subRel rel (trClos rel)

-- The transitive closure of set X must be transitive
transItive :: Rel Int -> Bool
transItive r = let tc = trClos (sort $ nub r) in
	transR tc

-- The transitive closure of set X must be the smallest set that has the 
-- two previous properties. 
-- The easiest way to verify this would be to take the superset of the
-- proposed TC and test for all subsets if none of them is a TC. However
-- we feel that that we can do this 'cheaper' using the following rationale: 
-- Every element in the set must be there for the set to be a TC.
-- We test this by checking for all 
-- subsets of the TC which are one element shorter than the TC, that that
-- set is either not a superset of X or not transitive.	
transSmallest :: Rel Int -> Bool
transSmallest r = let rel = sort $ nub r in
	all (\x -> not ((subRel rel x) && (transR x))) (dropOneSubLists (trClos rel))
	
propNonDupl :: (Rel Int -> Rel Int) -> Rel Int ->Bool
propNonDupl f r = let x = f (sort $ nub r) in x == nub x

propSorted :: (Rel Int -> Rel Int) -> Rel Int ->Bool
propSorted f r = let x = f (sort $ nub r) in x == sort x

-- TC of SC == SC of TC
prop_Q8 :: Rel Int -> Bool
prop_Q8 r = trClos (symClos r) == symClos (trClos r)
-- *TestRel> quickCheck prop_Q8 
-- *** Failed! Falsifiable (after 2 tests and 1 shrink):    
-- [(0,1)]

{-- function below is for debugging / informational purposes only. 
	We are fairly sure that this is not the right way to do this, 
	but it is convenient to have a function that runs all the tests
--}
testRelProperties :: IO()
testRelProperties = do 
	-- Symmetric closure
	print "Symmetric closure is superset of input set"
	a <- (quickCheckWith stdArgs { maxSuccess = 1000 } symSubSet)
	print "Symmetric closue is symmetric"
	b <- (quickCheckWith stdArgs { maxSuccess = 1000 } symMetric)
	print "Symmetric closure is smallest"
	c <- (quickCheckWith stdArgs { maxSuccess = 1000 } symSmallest) 
	print "Symmetric closure does not contain duplicates"
	d <- (quickCheckWith stdArgs { maxSuccess = 1000 } (propNonDupl symClos)) 
	print "Symmetric closure is sorted"
	e <- (quickCheckWith stdArgs { maxSuccess = 1000 } (propSorted symClos)) 
	-- Transitive closure
	print "Transitive closure is superset of input set"
	f <- (quickCheckWith stdArgs { maxSuccess = 1000 } transSubSet)
	print "Transitive closure is transitive"
	g <- (quickCheckWith stdArgs { maxSuccess = 1000 } transItive)
	print "Transitive closure is smallest"
	h <- (quickCheckWith stdArgs { maxSuccess = 1000 } transSmallest)
	print "Transitive closure does not contain duplicates"
	i <- (quickCheckWith stdArgs { maxSuccess = 1000 } (propNonDupl trClos)) 
	print "Transitive closure is sorted"
	j <- (quickCheckWith stdArgs { maxSuccess = 1000 } (propSorted trClos)) 
	return a



