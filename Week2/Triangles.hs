module Triangles where
 
import Data.List 
import System.Random

{-
Concise report
-----------------------
Time spent:
Less than 1 hour creating triangle function
About 9 hours creating test functions. Why so long? It's difficult if you want to generate random tests and you don't know anything about monads and IO.

Tests testing every different shape that's defined in data Shape with 100 random inputs. 

What can still be improved. Some tests are still weak and don't test every possibility.
-}

data Shape = NoTriangle | Equilateral 
            | Isosceles  | Rectangular | Other deriving (Eq,Show)
            

triangleMap :: [Integer] -> Shape
triangleMap (a:b:c:xs) = triangle a b c


triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c | a + b <= c || a + c <= b || b + c <= a = NoTriangle -- geen driehoek
               | a == b && b == c = Equilateral --Gelijkzijdig
               | a^2 + b^2 == c^2 || a^2 + c^2 == b^2 || b^2 + c^2 == a^2 = Rectangular --rechthoekig
               | a == b || a == c || b == c = Isosceles -- Gelijkbenig
               | otherwise = Other -- Anders (wel een driehoek)



getRandIsosceles :: IO [Integer]
getRandIsosceles = do 
  side <- getRandomInt2 50 100
  otherSide <- getRandomInt2 50 100
  return [x | x <- [toInteger side,toInteger side,toInteger side +1]]

getRandNoTriangle :: IO [Integer]
getRandNoTriangle = do 
  sideA <- getRandomInt 50
  sideB <- getRandomInt 50
  sideC <- getRandomInt 50
  return [x | x <- [toInteger sideA,toInteger sideB,0]]

getRandRectangular :: IO [Integer]
getRandRectangular = do 
  sideA <- getRandomInt2 3 3
  sideB <- getRandomInt2 4 4
  sideC <- getRandomInt2 5 5
  return [toInteger sideA,toInteger sideB, toInteger sideC]



getRandOther :: IO [Integer]
getRandOther = do 
  sideA <- getRandomInt2 51 51
  sideB <- getRandomInt2 56 56
  sideC <- getRandomInt2 8 8
  return [toInteger sideA,toInteger sideB,toInteger sideC]



getRandEquilateral :: IO [Integer]
getRandEquilateral = do 
  side <- getRandomInt 50
  return [toInteger side,toInteger side,toInteger side]


getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (1,n))

getRandomInt2  :: Int -> Int -> IO Int
getRandomInt2 a b = getStdRandom (randomR (a,b))
    

testRT :: Integer -> Integer -> IO [Integer] -> ([Integer] -> Shape)
                     -> ([Integer] -> Shape -> Bool) -> IO ()
testRT k n i f r = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- i
                  if r xs (f xs) then
                    do print ("pass on: " ++ show xs)
                       testRT (k+1) n i f r
                  else error ("failed test on: " ++ show xs)
                  

testPostTriangle :: IO [Integer] ->([Integer] -> Shape) -> (Shape -> Bool) -> IO ()
testPostTriangle i f p = testRT 1 100 i f (\_ -> p)

prop_equilateral :: Shape -> Bool
prop_equilateral shape | shape == Equilateral = True | otherwise = False

prop_isosceles :: Shape -> Bool
prop_isosceles shape | shape == Isosceles = True | otherwise = False

prop_notriangle :: Shape -> Bool
prop_notriangle shape | shape == NoTriangle = True | otherwise = False

prop_rectangular :: Shape -> Bool
prop_rectangular shape | shape == Rectangular = True | otherwise = False

prop_triangle :: Shape -> Bool
prop_triangle shape | shape == Other = True | otherwise = False


test_prop_equilateral =  testPostTriangle getRandEquilateral triangleMap prop_equilateral
test_prop_isosceles =  testPostTriangle getRandIsosceles triangleMap prop_isosceles
test_prop_notriangle =  testPostTriangle getRandNoTriangle triangleMap prop_notriangle
test_prop_rectangular =  testPostTriangle getRandRectangular triangleMap prop_rectangular
test_prop_triangle =  testPostTriangle getRandOther triangleMap prop_triangle
