module TestFunctions where

import System.Random

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

randomFlip :: Int -> IO Int
randomFlip x = do 
   b <- getRandomInt 1
   if b==0 then return x else return (-x)

genIntList :: IO [Int]
genIntList = do 
  k <- getRandomInt 20
  n <- getRandomInt 10
  getIntL k n
 
getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
   x <-  getRandomInt k
   y <- randomFlip x
   xs <- getIntL k (n-1)
   return (y:xs)

testR :: Int -> Int -> ([Int] -> [Int])
                    -> ([Int] -> [Int] -> Bool) -> IO ()
testR k n f r = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- genIntList
                  if r xs (f xs) then
                    do print ("pass on: " ++ show xs)
                       testR (k+1) n f r
                  else error ("failed test on: " ++ show xs)

testPost :: ([Int] -> [Int]) -> ([Int] -> Bool) -> IO ()
testPost f p = testR 1 100 f (\_ -> p)

samelength :: [Int] -> [Int] -> Bool
samelength xs ys = length xs == length ys

testRel :: ([Int] -> [Int]) -> ([Int] -> [Int] -> Bool) -> IO ()
testRel f r = testR 1 100 f r 

--stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
--stronger xs p q = forall xs (\ x -> p x ==> q x)
--weaker   xs p q = stronger xs q p 

neg :: (a -> Bool) -> a -> Bool
neg p = \ x -> not (p x)
