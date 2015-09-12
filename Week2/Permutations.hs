{-# OPTIONS_GHC -Wall #-}
module Permutations where

import Data.List
import TestFunctions

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation xs (y:ys) | length xs /= length (y:ys) = False
                        | otherwise = isPermutation (delete y xs) ys
isPermutation _ _ = False

{-

  http://www.cl.cam.ac.uk/~mjcg/Teaching/2011/Hoare/Notes/Notes.pdf

  We say {P} C {Q} is true, if whenever C is executed in a state satisfying
  P and if the execution of C terminates, then the state in which C’s execution
  terminates satisfies Q.

  Vrij vertaald door Jack:

  Als de functie C draait met de voorwaarden van P, en als het programma C
  beeindigd in een conditie waaraan Q voldoet, zeggen we dat {P} C {Q} (Hoare Triple)
  waar (True) is.

-}

--- Argument #1 : Number of tests to be generated
test :: Int -> IO()
test n =  testPermutations 1 n isPermutation

dummyIntList :: IO [Int]
dummyIntList = return [10,10,10]

--- Argument #1 : Starting for number of testcases
--- Argument #2 : End for number of testcases
--- Argument #3 : Function to be tested
testPermutations :: Int -> Int -> ([Int] -> [Int] -> Bool) -> IO()
testPermutations p q r = if p == q then print(show q ++ " Tests passed")
                         else do
                           v <- genIntList -- Change this to : [a]
                           i <- genIntList -- Change this to : [a]
                           if r v i || not(r v i) then
                              do print("Pass on: | " ++ show (r v i) ++ " | " ++ show v ++ " , " ++ show i ++ " |")
                                 testPermutations(p+1) q r
                           else error(" Failed test on input list #1: " ++ show v)
