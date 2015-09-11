module Lab2 where
 
import Data.List
import System.Random



data Shape = NoTriangle | Equilateral 
            | Isosceles  | Rectangular | Other deriving (Eq,Show)
            

triangleMap :: [Int] -> Shape
triangleMap (a:b:c:xs) = triangle a b c


triangle :: Int -> Int -> Int -> Shape
triangle a b c | a + b <= c || a + c <= b || b + c <= a = NoTriangle -- geen driehoek
               | a == b && b == c = Equilateral --Gelijkzijdig
               | a^2 + b^2 == c^2 || a^2 + c^2 == b^2 || b^2 + c^2 == a^2 = Rectangular --rechthoekig
               | a == b || a == c || b == c = Isosceles -- Gelijkbenig
               | otherwise = Other -- Anders (wel een driehoek)
               


getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

randomFlip :: Int -> IO Int
randomFlip x = do 
    b <- getRandomInt 1
    if b==0 then return x else return (-x)
               

genIntList :: IO [Int]
genIntList = do 
   k <- getRandomInt 20
   getIntL k 3
 
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




prop_other :: Int -> Int -> Int -> Bool
prop_other a b c = a + b + c == 20







