module Exercise4Debug
where 
import Lecture5

gridExercise1 :: Grid
gridExercise1 = [[0,0,0,3,0,0,0,0,0],
              [0,0,0,7,0,0,3,0,0],
              [2,0,0,0,0,0,0,0,8],
              [0,0,6,0,0,5,0,0,0],
              [0,9,1,6,0,0,0,0,0],
              [3,0,0,0,7,1,2,0,0],
              [0,0,0,0,0,0,0,3,1],
              [0,8,0,0,4,0,0,0,0],
              [0,0,2,0,0,0,0,0,0]]

emptyBlockHelper :: Int -> Int -> Sudoku -> Sudoku
emptyBlockHelper 3 3 sudoku = eraseS sudoku (3,3)
emptyBlockHelper 3 6 sudoku = eraseS sudoku (3,6)
emptyBlockHelper 3 9 sudoku = eraseS sudoku (3,9)
emptyBlockHelper 6 3 sudoku = eraseS sudoku (6,3)
emptyBlockHelper 6 6 sudoku = eraseS sudoku (6,6)
emptyBlockHelper 6 9 sudoku = eraseS sudoku (6,9)
emptyBlockHelper 9 3 sudoku = eraseS sudoku (9,3)
emptyBlockHelper 9 6 sudoku = eraseS sudoku (9,6)
emptyBlockHelper 9 9 sudoku = eraseS sudoku (9,9)
emptyBlockHelper r 3 sudoku = emptyBlockHelper (r+1) (1) (eraseS sudoku (r,3))
emptyBlockHelper r 6 sudoku = emptyBlockHelper (r+1) (4) (eraseS sudoku (r,6))
emptyBlockHelper r 9 sudoku = emptyBlockHelper (r+1) (7) (eraseS sudoku (r,9))
emptyBlockHelper r c sudoku = emptyBlockHelper r (c+1) (eraseS sudoku (r,c))
 

 

emptyBlockA::Sudoku -> Sudoku
emptyBlockA aSudoku = emptyBlockHelper 1 1 aSudoku

emptyBlockB::Sudoku -> Sudoku
emptyBlockB aSudoku = emptyBlockHelper 1 4 aSudoku

emptyBlockC::Sudoku -> Sudoku
emptyBlockC aSudoku = emptyBlockHelper 1 7 aSudoku

emptyBlockD::Sudoku -> Sudoku
emptyBlockD aSudoku = emptyBlockHelper 4 1 aSudoku

emptyBlockE::Sudoku -> Sudoku
emptyBlockE aSudoku = emptyBlockHelper 4 4 aSudoku

emptyBlockF::Sudoku -> Sudoku
emptyBlockF aSudoku = emptyBlockHelper 4 7 aSudoku

emptyBlockG::Sudoku -> Sudoku
emptyBlockG aSudoku = emptyBlockHelper 7 1 aSudoku

emptyBlockH::Sudoku -> Sudoku
emptyBlockH aSudoku = emptyBlockHelper 7 4 aSudoku

emptyBlockI::Sudoku -> Sudoku
emptyBlockI aSudoku = emptyBlockHelper 7 7 aSudoku

main2 :: IO ()
main2 = do 
	[r] <- rsolveNs [emptyN]
	showNode r
	s  <- genProblem2 r
	showNode s


eraser :: Sudoku -> Sudoku
eraser s  = emptyBlockA $ emptyBlockB $ emptyBlockC $ emptyBlockD $ emptyBlockE $ emptyBlockF $ emptyBlockG $ emptyBlockH (emptyBlockI s)
            


eraseDiagonal s  = emptyBlockA $ emptyBlockE $ (emptyBlockI s)

diagonalGrid = (sud2grid (eraseDiagonal sudokuExc4))
threeBlockSolution = solveAndShow diagonalGrid
















{- debug stuff here below -}


exampleExc4 :: Grid
exampleExc4 = [[ 9, 8, 5, 3, 6, 4, 7, 1, 2],
            [6, 2, 7, 1, 8, 5, 9, 4, 3],
            [4, 3, 1, 2, 7, 9, 5, 6, 8],
            [2, 5, 9, 8, 4, 3, 6, 7, 1],
            [8, 7, 6, 5, 2, 1, 4, 3, 9],
            [3, 1, 4, 6, 9, 7, 8, 2, 5],
            [7, 9, 3, 4, 1, 8, 2, 5, 6],
            [1, 4, 2, 9, 5, 6, 3, 8, 7],
            [5, 6, 8, 7, 3, 2, 1, 9, 4 ]]

sudokuExc4 :: Sudoku
sudokuExc4 = grid2sud exampleExc4

threeBlocksDiagonal = genProblem (emptyBlockA $ emptyBlockE $ emptyBlockI sudokuExc4,[]) >>= showNode      
bb = genProblem (emptyBlockF $ emptyBlockD $ emptyBlockC $ emptyBlockH sudokuExc4,[]) >>= showNode      




emptier = (emptyBlockF $ emptyBlockD $ emptyBlockC $ emptyBlockH sudokuExc4,(constraints sudokuExc4))
temp = eraser sudokuExc4
emptier2 :: Node
emptier2 = (temp,(constraints sudokuExc4))
oplossing = uniqueSol emptier2


nrcExample :: Grid
nrcExample = [[0,0,0,3,0,0,0,0,0],
              [0,0,0,7,0,0,3,0,0],
              [2,0,0,0,0,0,0,0,8],
              [0,0,6,0,0,5,0,0,0],
              [0,9,1,6,0,0,0,0,0],
              [3,0,0,0,7,1,2,0,0],
              [0,0,0,0,0,0,0,3,1],
              [0,8,0,0,4,0,0,0,0],
              [0,0,2,0,0,0,0,0,0]]
nrcSudoku :: Sudoku
nrcSudoku = grid2sud nrcExample

solveAndShowNrcPuzzle = solveAndShow nrcExample


