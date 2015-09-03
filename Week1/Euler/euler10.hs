{-# OPTIONS_GHC -Wall #-}
module Euler10 where

primes = sieve [2..]  -- The example haskell provided, trial divison to n so this takes ages
  where
    sieve (p:xs) = p : sieve [x | x <- xs, rem x p /= 0] 

solution::Integer -> Integer
solution x = sum (takeWhile (< x ) primes)
