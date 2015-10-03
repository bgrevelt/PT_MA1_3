module Exercise4
where 
import Lecture5
import Data.Foldable
import Data.List



block123 = blocks!!0
block456 = blocks!!1
block789 = blocks!!2

blockA = [(x,y) | x <- block123, y <- block123]
blockB = [(x,y) | x <- block123, y <- block456]
blockC = [(x,y) | x <- block123, y <- block789]
blockD = [(x,y) | x <- block456, y <- block123]
blockE = [(x,y) | x <- block456, y <- block456]
blockF = [(x,y) | x <- block456, y <- block789]
blockG = [(x,y) | x <- block789, y <- block123]
blockH = [(x,y) | x <- block789, y <- block456]
blockI = [(x,y) | x <- block789, y <- block789]




solutionThreeEmptyBlocks = genSingleSudokuWithEmptyBlocks (blockA ++ blockE ++ blockI)

proof4 =                [[0, 0, 0, 4, 6, 8, 7, 9, 1],
                          [0, 0, 0, 3, 0, 9, 8, 0, 4],
                          [0, 0, 0, 1, 7, 2, 3, 5, 6],
                          [0, 0, 0, 2, 9, 7, 1, 4, 3],
                          [0, 0, 0, 5, 0, 6, 2, 0, 8],
                          [0, 0, 0, 8, 4, 1, 5, 6, 9],
                          [1, 2, 6, 0, 0, 0, 0, 0, 0],
                          [7, 0, 3, 0, 0, 0, 0, 0, 0],
                          [9, 8, 4, 0, 0, 0, 0, 0, 0]]


solutionFourEmptyBlocks = do 
	showGrid proof4
	solveAndShow proof4



generatingFourEmptyBlocks = genMultipleSudokusWithEmptyBlocks setFourEmptyBlocks
generatingFiveEmptyBlocks = genMultipleSudokusWithEmptyBlocks setFiveEmptyBlocks

genSingleSudokuWithEmptyBlocks :: [(Row,Column)] -> IO ()
genSingleSudokuWithEmptyBlocks constr = do 
	[r] <- rsolveNs [emptyN]
	showNode r
	s  <- generateProblem r constr
	showNode s

generateProblem :: Node -> [(Row,Column)] -> IO Node
generateProblem n constr = do 
	ys <- randomize xs
	return (minimalize n ys)
	where xs = constr



genMultipleSudokusWithEmptyBlocks [] = print "done with generating"
genMultipleSudokusWithEmptyBlocks (x:xs) = do 
	genSingleSudokuWithEmptyBlocks (concat x)
	genMultipleSudokusWithEmptyBlocks xs


powerset :: Foldable t => t a -> [[a]]
powerset = Data.Foldable.foldr (\x acc -> acc ++ map (x:) acc) [[]]
blocksA_I = [blockA, blockB, blockC, blockD, blockE, blockF, blockG, blockH, blockI]
setFiveEmptyBlocks = filter(\x -> length x == 5) (powerset blocksA_I)
setFourEmptyBlocks = filter(\x -> length x == 4) (powerset blocksA_I)
