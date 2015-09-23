{-# LANGUAGE FlexibleInstances #-}

module Lab4
where
import SetOrd
import Data.List
import System.Random
import Test.QuickCheck


getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

randomFlip :: Int -> IO Int
randomFlip x = do 
    b <- getRandomInt 1
    if b==0 then return x else return (-x)
               

genIntSet :: IO (Set Int)
genIntSet = do 
   k <- getRandomInt 20
   getIntS k 9
 
getIntS :: Int -> Int ->  IO (Set Int)
getIntS _ 0 = return emptySet
getIntS k n = do 
    x <-  getRandomInt k
    y <- randomFlip x
    xs <- getIntS k (n-1)
    return (unionSet (list2set [y]) xs)


instance Arbitrary (Set Int) where
         arbitrary = do
              intList <- arbitrary :: Gen [Int]
              return (list2set intList)

differenceSet :: (Ord at) => Set at  -> Set at -> Set at
differenceSet s (Set [])  = s
differenceSet s (Set (i:s2)) = differenceSet (deleteSet i s) (Set s2)

unionSet2 :: (Ord at) => Set at -> Set at -> Set at
unionSet2 s1 (Set [])  =  s1
unionSet2 s1 (Set (i:s2)) = insertSet i (unionSet s1 (Set s2) )

{-
differenceSet of differenceSet is intersect :)
-}
intersectSet :: (Ord a) => Set a -> Set a -> Set a
intersectSet a b = differenceSet a ((differenceSet a b))

{- 
If an element of s2 is in s1 , then add that element to the result set and else you don't.
Till we recursively went through the list.
-}
intersectSet2 :: (Ord at) => Set at -> Set at -> Set at
intersectSet2 s1 (Set []) =  Set []
intersectSet2 s1 (Set (e:s2))    | inSet e s1 = insertSet e (intersectSet s1 (Set s2))
                                | True = intersectSet s1 (Set s2) 

isProperSet :: (Set Int -> Set Int -> Set Int) -> Set Int -> Set Int -> Bool
isProperSet f (Set a) (Set b) = let (Set y) = f (Set a) (Set b) in
	list2set y == (Set y)

test :: IO()
test = do 
	-- union
	a <- (quickCheck (isProperSet unionSet2))
	return a
