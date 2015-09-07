module Derangement where

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

deran :: Int -> [[Int]]
deran n = filter (\ p -> 
  isDerangement p [0..n-1]) 
  (perms [0..n-1])

count :: Eq a => a -> [a] -> Int                                  -- Use this for dupes
count x [] = 0
count x (y:ys) | x == y = 1 + (count x ys)
               | otherwise = count x ys

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement xs ys = and 					  -- Take the AND of the list
  [index x xs /= index x ys                                       -- Index of x != other index x
  && count x ys == 1 && count x xs == 1                           -- Dupes are scary
  | x <- xs ]                                                     -- Take x from xs      
  where
    index n (x:xs) | n == x = 0 | otherwise = 1 + index n xs      -- This is space sensitive :")

