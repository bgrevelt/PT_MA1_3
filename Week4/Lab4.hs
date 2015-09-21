module Lab4
where
import SetOrd
import Data.List
import System.Random


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