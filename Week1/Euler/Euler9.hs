{-# OPTIONS_GHC -Wall #-}
module Euler9 where

solution::Integer -> [Integer]
solution n = [
           a * b * c 
           | a <- [1..n], 
             b <- [(a + 1)..(n - (a + 1))],   -- We can do this because a < b < c 
             c <- [(b + 1)..(n - (b + 1))],   -- The minus is to avoid a sum > n
             a + b + c == n,
             a * a + b * b == c * c
       ]

