import Data.Bits

import Lecture6
-- https://www.khanacademy.org/computing/computer-science/cryptography/modarithmetic/a/fast-modular-exponentiation
-- https://www.youtube.com/watch?v=EHUgNLN8F1Y
{-
 Each time, we are going to double and add 1 to the base and the exponential.
-}
expMTest :: Integer -> Integer
expMTest e = expM (2) (e*2 + 1) 50

expMTest' :: Integer -> Integer
expMTest' e = exMOpt (2) (e*2 + 1) 50
