{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module TestRel

where 
  
import Closures
import Data.List
import Test.QuickCheck

{------------------- Generator ----------------}
{--
The way I have implemented automatic generation by quickCheck 
may not be optimal, so allow me to explain:
At first I tried to use QuickChecks standard generator to generate
Rel's. This works, but the result contains duplicates. As I could not
get my custom generator to work, I decided to continue with the standard
generator and sort and remove duplicates in each testable property.

Now I'm at a point where I finally managed to get a generator up and running
but if I understand correctly, we will need to use newtype for the relation
instead of type because otherwise QuickCheck will not be able to distinguish
between [(a),(a)] and Rel a. The problem was that if I change
type Rel a = Rel [(a,a)] to newtype Rel a = Rel [(a,a)], I will need to change 
all code that uses relation by adding (Rel <var>) around all Rel type variables
This is a lot of work which does not really seem to bring us any benefits. 
Quite the opposite, it makes handing the Rel type as as the standard [(a,a)] type
harder sometimes.
To work around this, I create a new type "Relation" which is basically the same
type as Rel. I will define this as a new type and only use it as input parameter
for testable properties. This way quickcheck will use my generator for all the
testable properties without the need to change all of my code to use the new type
--}

newtype Relation a = Relation [(a, a)] deriving (Eq,Ord)

instance (Show a) => Show (Relation a) where
    showsPrec _ (Relation s) str = showRel s str

showRel []     str = showString "{}" str
showRel (x:xs) str = showChar '{' ( shows x ( showl xs str))
     where showl []     str = showChar '}' str
           showl (x:xs) str = showChar ',' (shows x (showl xs str))
		   

instance Arbitrary (Relation Int) where
         arbitrary = do
              rs <- arbitrary :: Gen [(Int,Int)]
              return (Relation (sort $ nub rs))
 
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
symSubSet r = subRel r (symClos r)


-- The symmetric closure of set X must be symmetric	
symMetric :: Relation Int -> Bool
symMetric (Relation r) =
	all (\(x,y) -> (elem (y,x) (symClos r))) r

-- The symmetric closure of set X must be the smallest possilbe set for 
-- which the previous two properties hold. We check this by making sure
-- that for each element (x,y) in the SC, either (x,y) or (y,x) is part of X	
symSmallest :: Relation Int -> Bool
symSmallest (Relation r) = 
	all (\(x,y) -> ((elem (y,x) r) || (elem (x,y) r))) (symClos r)


{-- Transitive closure properties for quickcheck --}

-- Set X must be a subset of the transitive closure of set X
transSubSet :: Relation Int -> Bool
transSubSet (Relation r) =
	subRel r (trClos r)

-- The transitive closure of set X must be transitive
transItive :: Relation Int -> Bool
transItive (Relation r) = transR (trClos r)

-- The transitive closure of set X must be the smallest set that has the 
-- two previous properties. In other words: every element in the set must
-- be there for the set to be a TC. We test this by checking for all 
-- subsets of the TC which are one element shorter than the TC, that that
-- set is either not a superset of X or not transitive.	
transSmallest :: Relation Int -> Bool
transSmallest (Relation r) = 
	all (\x -> not ((subRel r x) && (transR x))) (dropOneSubLists (trClos r))
	
propNonDupl :: (Rel Int -> Rel Int) -> Relation Int ->Bool
propNonDupl f (Relation r) = r == nub r

propSorted :: (Rel Int -> Rel Int) -> Relation Int ->Bool
propSorted f (Relation r) = r == sort r

-- TC of SC == SC of TC
prop_Q8 :: Relation Int -> Bool
prop_Q8 (Relation r) = trClos (symClos r) == symClos (trClos r)
-- *TestRel> quickCheck prop_Q8 
-- *** Failed! Falsifiable (after 2 tests and 1 shrink):    

test :: IO()
test = do 
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



