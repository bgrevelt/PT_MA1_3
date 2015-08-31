module CSI where
import Data.List
data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]


says :: Boy -> Boy -> Bool
says p1 p2 | p1==Jack && p2 == Matthew = False
           | p1==Jack && p2 == Peter = False
           | p1==Carl && p2 == Arnold = False
           | p1==Arnold && (p2 == Peter || p2 == Matthew) = False
           | otherwise = True
 
accusers :: Boy -> [Boy]
accusers Peter = [Matthew, Jack]
accusers Matthew = [Peter, Jack, Arnold]
accusers Jack = []
accusers Carl = []
accusers Arnold = []


guilty, honest :: [Boy]
guilty = []
honest = []


powerset = foldr (\x acc -> acc ++ map (x:) acc) [[]]

-- all possible solutions
options = powerset boys

-- filter on three people speak the truth
only3 = filter(\x -> length x == 3) options







-- Below this is my playground
b = accusers Matthew
c = accusers Peter




genIntersect :: Eq a => [[a]] -> [a]
genIntersect [e] = e
genIntersect (e:l) = intersect (genIntersect l) e 





--als Peter de waarheid spreekt, dan
--spreekt Jack en Matthew niet de waarheid

hoi :: Boy -> [Boy]
hoi b = hoi' boys b

hoi'  :: [Boy] -> Boy -> [Boy]
hoi' [] b2 = []
hoi' (b:bs) b2 = if (elem b2 (filter (says b2) bs)) 
                 then [b] ++ hoi' bs b2 
                 else hoi' bs b2
