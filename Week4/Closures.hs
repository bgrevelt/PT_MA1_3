{--
      Time spent: 2 hours
--}

module Closures where

import Data.Tuple
import Data.List

type Rel a = [(a, a)]

symClos :: Ord a => Rel a -> Rel a
symClos x = sort (nub $ x ++ map swap x)

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = 
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]
  
trClos :: Ord a => Rel a -> Rel a
trClos x | x == rest = sort x
         | otherwise = trClos rest 
         where rest = nub (x ++ (x @@ x))       -- Probably want us to use the operator, would prefer using nub once
         
{-- 
Alternative version. The theory is that this one should be faster than the other implementation
because it composes with the input set, rather than the result of the previous step
theory is the key word here since in practice the first algorithm is faster :') 
trClos' :: Ord a => Rel a -> Rel a
trClos' i = trClos'' i [] i where
	trClos'' orig beforeLast last = if beforeLast == last then last else
		trClos'' orig last (nub (last ++ (last @@ orig)))
--}
