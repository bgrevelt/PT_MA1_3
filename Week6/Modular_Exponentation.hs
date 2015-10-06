import Data.Bits

import Lecture6
-- https://www.khanacademy.org/computing/computer-science/cryptography/modarithmetic/a/fast-modular-exponentiation
-- https://www.youtube.com/watch?v=EHUgNLN8F1Y

{-
  First I read some documentation on modular exponentation, then implemented the method in Lecture 6 and
  then I decided to run some tests. Before these methods, in ghci, I did the following command:
  :set +s
  After that, the running time and the memory footprint of the method is shown. That is what I will be showing in
  this document.
-}

{-

  Because the method beneath wouldn't run, because m10 already was too big of a footprint, I was
  profiling with smaller digits.

  *Main> prime_test_F_Unoptimized m7
  True
  (0.11 secs, 4,663,464 bytes)


-}
testNaive :: IO ()
testNaive = do
            a <- (prime_test_F_Unoptimized m1)
            b <- (prime_test_F_Unoptimized m2)
            c <- (prime_test_F_Unoptimized m3)
            d <- (prime_test_F_Unoptimized m4)
            e <- (prime_test_F_Unoptimized m5)
            f <- (prime_test_F_Unoptimized m6)
            g <- (prime_test_F_Unoptimized m7)
            h <- (prime_test_F_Unoptimized m8) -- It already stucks at this one..
            print "Testing for primes beginning with m1 to m8 with the naive algorithm."
            print ([a,b,c,d,e,f,g,h])

{-
  Because I want to test whether I had made an optimization, I decided to start where I left off
  with the naive method. It took way to long with the naive method to even calculate whether m8 is a prime. My
  new method starts with m8 and goes all the way to m24 and it takes approximately 7~ seconds for m8 up to and including m24!

  *Main> testOptimized
  "Testing for primes beginning with m8 to m24 with the optimized algorithm."
  [True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True]
  (7.23 secs, 725,725,656 bytes)

  *Main> testOptimized
  "Testing for primes beginning with m8 to m24 with the optimized algorithm."
  [True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True]
  (7.07 secs, 724,880,920 bytes)

  *Main> testOptimized
  "Testing for primes beginning with m8 to m24 with the optimized algorithm."
  [True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True]
  (7.06 secs, 724,880,664 bytes)

  *Main> testOptimized
  "Testing for primes beginning with m8 to m24 with the optimized algorithm."
  [True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True]
  (7.17 secs, 724,880,376 bytes)

  *Main> testOptimized
  "Testing for primes beginning with m8 to m24 with the optimized algorithm."
  [True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True]
  (7.06 secs, 724,881,952 bytes)

-}

testOptimized :: IO ()
testOptimized = do
            a <- (prime_test_F m8)
            b <- (prime_test_F m9)
            c <- (prime_test_F m10)
            d <- (prime_test_F m11)
            e <- (prime_test_F m12)
            f <- (prime_test_F m13)
            g <- (prime_test_F m14)
            h <- (prime_test_F m15)
            i <- (prime_test_F m16)
            j <- (prime_test_F m17)
            k <- (prime_test_F m18)
            l <- (prime_test_F m19)
            m <- (prime_test_F m20)
            n <- (prime_test_F m21)
            o <- (prime_test_F m22)
            p <- (prime_test_F m23)
            q <- (prime_test_F m24)
            print "Testing for primes beginning with m8 to m24 with the optimized algorithm."
            print ([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q])
