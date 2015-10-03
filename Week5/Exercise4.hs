module Exercise4
where 
import Lecture5

blockA = [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
blockB = [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
blockC = [(1,7),(1,8),(1,9),(2,7),(2,8),(2,9),(3,7),(3,8),(3,9)]
blockD = [(4,1),(4,2),(4,3),(5,1),(5,2),(5,3),(6,1),(6,2),(6,3)]
blockE = [(4,4),(4,5),(4,6),(5,4),(5,5),(5,6),(6,4),(6,5),(6,6)]
blockF = [(4,7),(4,8),(4,9),(5,7),(5,8),(5,9),(6,7),(6,8),(6,9)]
blockG = [(7,1),(7,2),(7,3),(8,1),(8,2),(8,3),(9,1),(9,2),(9,3)]
blockH = [(7,4),(7,5),(7,6),(8,4),(8,5),(8,6),(9,4),(9,5),(6,6)]
blockI = [(7,7),(7,8),(7,9),(8,7),(8,8),(8,9),(9,7),(9,8),(9,9)]


genThreeEmptyBlocks :: IO ()
genThreeEmptyBlocks = do 
	[r] <- rsolveNs [emptyN]
	showNode r
	s  <- genProblem3 r
	showNode s

genProblem3 :: Node -> IO Node
genProblem3 n = do 
	ys <- randomize xs
	return (minimalize n ys)
	where xs = filledPositions3 (fst n)

filledPositions3 :: Sudoku -> [(Row,Column)]
filledPositions3 s = blockA ++ blockE ++ blockI	

genFourEmptyBlocks :: IO ()
genFourEmptyBlocks = do 
	[r] <- rsolveNs [emptyN]
	showNode r
	s  <- genProblem4 r
	showNode s

genProblem4 :: Node -> IO Node
genProblem4 n = do 
	ys <- randomize xs
	return (minimalize n ys)
	where xs = filledPositions4 (fst n)


filledPositions4 :: Sudoku -> [(Row,Column)]
filledPositions4 s = blockA ++ blockE ++ blockI ++ blockH

genFiveEmptyBlocks :: IO ()
genFiveEmptyBlocks = do 
	[r] <- rsolveNs [emptyN]
	showNode r
	s  <- genProblem5 r
	showNode s

genProblem5 :: Node -> IO Node
genProblem5 n = do 
	ys <- randomize xs
	return (minimalize n ys)
	where xs = filledPositions5 (fst n)

filledPositions5 :: Sudoku -> [(Row,Column)]
filledPositions5 s = blockA ++ blockE ++ blockI ++ blockH ++ blockB
