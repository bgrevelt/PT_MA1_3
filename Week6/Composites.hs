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
  printTest xs

--
-- What is the least composite number that you can find that fools the check, for prime_tests_F k with k=1,2,3?
--

{-- 
 "Where the check can go wrong is on classifying composite numbers; these can slip through the Fermat test."
 "What is the least composite number that you can find that fools the check"
 
 So: composite numbers can go wrong because they can slip through the test.
 So: There exists a composite number that is smaller then all other composite numbers and passes the Fermat test
 So: it's 4.
 
 For k = 1
   4
 For k = 2
   4
 For k = 3
   4
--}

-- What happens if you increase k?
-- When you increase k you make more iterations and it becomes more accurate.
