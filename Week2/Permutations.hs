{-# OPTIONS_GHC -Wall #-}
module Permutations where

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True                      --- Empty lists are always the same
isPermutation xs (y:ys) | length xs /= length (y:ys) = False
                        | otherwise = isPermutation (delete y xs) ys

--- If the legth is not the same, it has no purpose in checking further.
--- If listsizes are the same, proceed in deleting the first occurence
--- of an element of the second list from the first list. Keep doing this
--- until the list is empty.

--- The next step is generating testdata... Will be continued...
