module CSI where
import Data.List
import Data.Foldable

data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq, Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

says :: Boy -> Boy -> Bool
says Jack Matthew = False
says Jack Peter = False
says Carl Arnold = False
says Arnold Peter = False
says Arnold Matthew = False
says _ _ = True
 
accusers :: Boy -> [Boy]
accusers Peter = [Matthew, Jack]
accusers Matthew = [Peter, Jack, Arnold]
accusers Jack = []
accusers Carl = []
accusers Arnold = []

subsets :: Int -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets k (x:xs) = map (x:) (subsets (k - 1) xs) ++ subsets k xs

-- Source: Alex wrote this on Thursday during the summer school Functional Programming
genIntersect :: Eq a => [[a]] -> [a]
genIntersect [e] = e
genIntersect (e:l) = intersect (genIntersect l) e 

-- Get all possible worlds where three boys tell the truth and two are lying. In this case there are only 10 possibillities.

tenWorlds = subsets 3 boys

-- Checks whether the given list boys don't "say" that another person in the group is lying. 

allAgree:: [Boy] -> Bool
allAgree xs = length [x | x <- xs, y <- xs, says x y] == 9


guilty, honest :: [Boy]

-- filtering all lists out of boys where one or more boys say that some -- one else in the group doesn't "says" the truth
honestList = filter allAgree tenWorlds


-- gets the first one in the list
honest = head honestList

-- map makes for each honest person in the list the people he    accusses. Filter filter outs the list of the persons that don't accuse anybody. The last thing is to intersect all the sets of accussed people and shows thereby who is guilty
guilty = genIntersect (filter (not . null) (map (accusers) honest))
