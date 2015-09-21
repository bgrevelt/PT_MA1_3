  module Lecture2
  
  where 
  
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

  quicksort :: Ord a => [a] -> [a]  
  quicksort [] = []  
  quicksort (x:xs) = 
     quicksort [ a | a <- xs, a <= x ]  
     ++ [x]
     ++ quicksort [ a | a <- xs, a > x ]

  isTrue :: a -> Bool
  isTrue _ = True

  prop_ordered :: Ord a => [a] -> Bool
  prop_ordered [] = True
  prop_ordered (x:xs) = all (>= x) xs && prop_ordered xs

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

  quicksrt :: Ord a => [a] -> [a]  
  quicksrt [] = []  
  quicksrt (x:xs) = 
     quicksrt [ a | a <- xs, a < x ]  
     ++ [x]
     ++ quicksrt [ a | a <- xs, a > x ]

  samelength :: [Int] -> [Int] -> Bool
  samelength xs ys = length xs == length ys

  testRel :: ([Int] -> [Int]) -> ([Int] -> [Int] -> Bool) -> IO ()
  testRel f r = testR 1 100 f r 

  infix 1 ==> 
  
  (==>) :: Bool -> Bool -> Bool
  p ==> q = (not p) || q
  forall = flip all

  stronger, weaker :: [a] -> 
         (a -> Bool) -> (a -> Bool) -> Bool
  stronger xs p q = forall xs (\ x -> p x ==> q x)
  weaker   xs p q = stronger xs q p 

  neg :: (a -> Bool) -> a -> Bool
  neg p = \ x -> not (p x)

  infixl 2 .&&. 
  infixl 2 .||.

  (.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
  p .&&. q = \ x -> p x && q x 
 
  (.||.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
  p .||. q = \ x -> p x || q x

