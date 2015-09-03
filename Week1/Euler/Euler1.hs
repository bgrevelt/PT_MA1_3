module Euler1 where
solution :: Integer
solution = sum [number| number <- [1..1000], rem number 3 ==0 || rem number 5 ==0]

anotherSolution :: Integer
anotherSolution = sum (filter (\n -> n `mod` 5 == 0 || n `mod` 3 == 0) [1..999])
