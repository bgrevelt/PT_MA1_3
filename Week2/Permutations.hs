{-# OPTIONS_GHC -Wall #-}
module Permutations where

import Data.List
import TestFunctions

{-
  === Concise test report ===
  This is about as concise as I can be, so here it comes.
    - I've spent around 6 to 8 hours on this code.
    - I did not include reading and watching videos about the subject..
    - The most problematic part for me was understanding what exactly should
    be automatically tested. According to Hoare Logic - or at least how I interpreted
    it - a program is true when the precondition is met for some random input.
    Then when the function and the postcondition are met, the program is valid according
    to Hoare Logic. So there is a question left for me:
      - Should a function be (automatically) tested according to all possible preconditions?
      - That doesn't say anything about the correctness of the implementation of the function, so that is
      a bit of a dilemma for me... Or isn't that the goal of Hoar Logic?
-}

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
