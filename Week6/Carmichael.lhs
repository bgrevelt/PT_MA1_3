> module Carmichael where
        
> import Lecture6

The function to generate Carmichael numbers as given in the assignment

> carmichael :: [Integer]
> carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
>     k <- [2..], 
>     isPrime (6*k+1), 
>     isPrime (12*k+1), 
>     isPrime (18*k+1) ]

The test function. Pretty simple setup. We take k carmichael numbers and use the prime_tests_F function
to test each Carmichael number n times (prime_tests_F uses a random 'a' for each run, so multiple runs are 
useful) 
        Sidenote: Wouldn't it have been nicer if prime_tests_F would have picked n random numbers from the range [1..prime-1] 
        instead of generating a new random value each iteration? Now we have a chance of testing the same value multiple
        times. (Granted, with larger numbers this chance is pretty small)
The test concludes by printing the percentage of tried Carmichael numbers that have been identified as a prime
(or, to be fair, as a probable prime)

> testCarm :: Int -> Int -> IO ()
> testCarm k = testCarm' (take k carmichael)
> testCarm' :: [Integer] -> Int -> IO ()
> testCarm' [] _ = print "Done"
> testCarm' xs n = do
>     r <- sequence $ fmap (prime_tests_F n) xs
>     let 
>         pcntFail = 100 * (length $ filter id r) `div` (length r) in
>         print (show pcntFail ++ "% of carmichael numbers were incorrectly flagged as being prime when testing " ++ show n ++ " numbers in the range [1..prime-1]")
        
Some test output:
*Carmichael> testCarm 10 10
"90% of carmichael numbers were incorrectly flagged as being prime when testing 10 numbers in the range [1..prime-1]"
*Carmichael> testCarm 10 100
"40% of carmichael numbers were incorrectly flagged as being prime when testing 100 numbers in the range [1..prime-1]"
*Carmichael> testCarm 10 1000
"10% of carmichael numbers were incorrectly flagged as being prime when testing 1000 numbers in the range [1..prime-1]"
*Carmichael> testCarm 10 10000
"0% of carmichael numbers were incorrectly flagged as being prime when testing 10000 numbers in the range [1..prime-1]"

It makes sense that these Carmichael numbers are often 'flagged' as being prime, as that was what they were designed for.
The reason that we can, with due dilligence, mark them as non-prime is that the 'modular arithmetic congruence relation' 
only holds if the random number used by prime_test_F is not a coprime of the Carmichael number under test.

If we change the functions above a little bit, we can clearly see this: 
       
        
Carmichael numbers as before, but instead of only returning the number, also give us a tuple with the three primesused to create the number
       
> carmichael' :: [((Integer,Integer, Integer), Integer)]
> carmichael' = [ ((6*k+1,12*k+1,18*k+1), (6*k+1)*(12*k+1)*(18*k+1)) | 
>     k <- [2..], 
>     isPrime (6*k+1), 
>     isPrime (12*k+1), 
>     isPrime (18*k+1) ]
    
Slight modification to prime_test_F:
Test all numbers in the range [1..prime-1] instead of one random number from that range.
Return a list of all numbers for which the test failed

> failing_prime_test_F :: Integer -> [Integer]
> failing_prime_test_F n = 
>       [ a | a <- [1..(n-1)], (exM a (n-1) n /= 1)]
      
Now, we can see for which a's exM a (n-1) n is not equal to n. Our expectancy is that all of these numbers are products of the
factors of the Carmichael number that we put in. Let's make a function to check that

> allFactorOf :: (Integer,Integer,Integer) -> [Integer] -> Bool
> allFactorOf (fa,fb,fc) = all (\x -> fa `divs` x || fb `divs` x || fc `divs` x) where
>     divs :: Integer -> Integer -> Bool
>     divs a b = b `mod` a == 0 
   
And put all of this to the test:
*Carmichael> let cm1 = head carmichael'
(0.00 secs, 1031200 bytes)
*Carmichael> cm1
((37,73,109),294409)
*Carmichael> failing_prime_test_F $ snd cm1
[37,73,74,109,111,146,148,185,218,219,222,259,292,296,327,333,365,370,407,436,438,...,294372]
*Carmichael> allFactorOf (fst cm1) (failing_prime_test_F $ snd cm1)
True

Another interesting observation is that it seems to get 'harder' to detect a Carmichael number as a non-prime as the Carmichael number
gets larger. For that to be true, the percentage of numbers in the range [1..carmichael-1] which are not coprime to the carmichael number 
for growing carmichael numbers should decrease.
 
Filter all integers from a list of integers which are not factor of one of the integers in the
input tuple

> factorOf :: (Integer,Integer,Integer) -> [Integer] -> [Integer]
> factorOf (fa,fb,fc) = filter (\x -> fa `divs` x || fb `divs` x || fc `divs` x) where
>     divs :: Integer -> Integer -> Bool
>     divs a b = b `mod` a == 0 
    
Calculate the percentage of numbers in the range [2..'Carmichael number n'] which are factors
of one of the prime factors of the Carmichael number.

> pcntFct :: Int -> Double
> pcntFct n = let (facs,c) = carmichael'!!n in
>     realToFrac ( length $ factorOf facs [2..c]) / realToFrac c * 100.0

Calculate the percentage of numbers in the range [2..'Carmichael number'] which are factors
of one of the prime factors of the Carmichael number for the first n Carmichael numbers.  

> calcPercentages :: Int -> IO ()
> calcPercentages n = cp [0..n-1] where
>     cp [] = print "done"
>     cp (x:xs) = do
>         print $ show x ++ ": " ++ show ( pcntFct x) ++ "%"
>         cp xs

Let's put this to the test. Out expectation is that the percentage will drop as the Carmichael
number gets larger:

*Carmichael> calcPercentages 10
"0: 4.915950259672768%"
"1: 0.8676904796213669%"
"2: 0.675786981732555%"
"3: 0.596616214158226%"
"4: 0.5533947932127754%"
"5: 0.5435505125717432%"
"6: 0.30490092160665044%"
"7: 0.25207798310958957%"
"8: 0.15652284038808148%"

(terminated the run after 9 Carmichael numbers because 7 hours had passed)
