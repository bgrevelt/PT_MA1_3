{--
      Time spent: 2 hours
--}

import Data.Tuple
import Data.List

type Rel a = [(a, a)]

symClos :: Ord a => Rel a -> Rel a
symClos x = nub (sort $ x ++ (map swap x))

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = 
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]
  
trClos :: Ord a => Rel a -> Rel a
trClos x | x == rest = sort x
         | otherwise = trClos rest 
         where rest = nub (x ++ (x @@ x))       -- Probably want us to use the operator, would prefer using nub once
