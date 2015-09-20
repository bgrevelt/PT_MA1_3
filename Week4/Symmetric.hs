
import Data.tuple

type Rel a = [(a, a)]

symClos :: Ord a => Rel a -> Rel a
symClos x = x ++ map $ swap x
