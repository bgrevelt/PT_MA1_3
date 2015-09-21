
import Data.List
import Data.Tuple

type Rel a = [(a, a)]

symClos :: Ord a => Rel a -> Rel a
symClos x = sort $ x ++ (map swap x)
