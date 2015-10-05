module Composites
 
where

import Data.List
import Lecture6

{--
    Time spent: 7 hours
--}

composites :: [Integer]
composites = filter (not . isPrime) [2..]

printTest :: [Integer] -> IO ()
printTest (x : xs) = do
  a <- prime_tests_F 4 x
  print (a, x)
  b <- printTest xs 
  return b

--
-- What is the least composite number that you can find that fools the check, for prime_tests_F k with k=1,2,3?
--

{-- Gotta love random
 For k = 1
   4
 For k = 2
   4
 For k = 3
   4
--}

-- What happens if you increase k?
-- When you increase k you make more iterations and it becomes more accurate.
