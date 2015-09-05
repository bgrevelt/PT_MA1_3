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

-- source: http://rosettacode.org/wiki/Power_set#Haskell
powerset :: Foldable t => t a -> [[a]]
powerset = Data.Foldable.foldr (\x acc -> acc ++ map (x:) acc) [[]]


-- Source: Alex wrote this on Thursday during the summer school Functional Programming
genIntersect :: Eq a => [[a]] -> [a]
genIntersect [e] = e
genIntersect (e:l) = intersect (genIntersect l) e 

-- Filters on all possible worlds so that you only have the 
-- world left where three boys tell the truth and two are lying. 

-- All possible worlds are retrieved with "powerset boys". 
-- This returns in total 32 possibillities (2^5). 
-- After the filter is applied there only 10 possibillities left.

tenWorlds = filter(\x -> length x == 3) (powerset boys)

-- Checks whether the given list boys don't "say" that another person in the groep is lying. 
allAgree:: [Boy] -> Bool
allAgree xs = length [x | x <- xs, y <- xs, says x y] == 9


guilty, honest :: [Boy]

-- filtering all lists out of boys where one or more boys say that some -- one else in the group doesn't "says" the truth
honestList = filter allAgree tenWorlds


-- gets the first one in the list
honest = head honestList

-- map makes for each honest person in the list the people he    accusses. Filter filter outs the list of the persons that don't accuse anybody. The last thing is to intersect all the sets of accussed people and shows thereby who is guilty
guilty = genIntersect (filter (not . null) (map (accusers) honest))
