module Setup where

type Row        = Int
type Column     = Int
type Value      = Int
type EmptyValue = Int
type Grid       = [[Value]]

rows :: [Int]
rows    = [1..9]

columns :: [Int]
columns = [1..9]

numberToEmptyValue :: Int -> Int
numberToEmptyValue _ = 0

isUniqueInRow :: Grid -> Row -> Value -> Bool
isUniqueInRow _ _ _ = False

isUniqueInColumn :: Grid -> Column -> Value -> Bool
isUniqueInColumn _ _ _ = False

isUniqueInBlock :: Grid -> [Value] -> Value -> Bool
isUniqueInBlock _ _ _ = False
