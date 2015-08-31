module CSI where
import Data.List
data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]


says :: Boy -> Boy -> Bool
--says Matthew Matthew :: False
--says Matthew Carl :: False
--says Peter Matthew :: True
says p1 p2 | p1==Jack && p2 == Matthew = False
           | p1==Jack && p2 == Peter = False
           | p1==Carl && p2 == Arnold = False
           | p1==Arnold && (p2 == Peter || p2 == Matthew) = False
            | otherwise = True
            

 

accusers :: Boy -> [Boy]
accusers Peter = [Matthew, Jack]
accusers Matthew = [Peter, Jack, Arnold]
accusers Jack =   boys
accusers Carl = boys
accusers Arnold = boys


guilty, honest :: [Boy]
guilty = []
honest = []


a = [[True, True, True, True, True], [True, True, True, True, False], [True, True, True, False, False]]


b = accusers Matthew
c = accusers Peter

