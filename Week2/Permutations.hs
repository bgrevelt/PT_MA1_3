{-# OPTIONS_GHC -Wall #-}
module Permutations where

import Data.List

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation xs (y:ys) | length xs /= length (y:ys) = False
                        | otherwise = isPermutation (delete y xs) ys
isPermutation _ _ = False
