{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = mod n 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = div n 10


-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits n = if n <= 0 then [] -- Base case
	        else [lastDigit n] ++ toRevDigits (dropLastDigit n) -- Add the lastdigit before the next
	        -- [Bouke] Why not lastDigit n :: toRevDigits (dropLastDigit n) ?


-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (n1 : n2 : ns) = n1 : 2 * n2 : doubleEveryOther ns -- Take 2 integers, multiply the second, continue
doubleEveryOther rest = rest  -- If there are 0/1 left return this.


-- Exercise 4 -----------------------------------------

--- Break down a multi number digit.
--- For example: 123 would become [1,2,3]
splitMultiNumberDigit :: Integer -> [Integer]
splitMultiNumberDigit = map (read . (:[])) . show

-- Calculate the sum of all the digits in every Integer.
-- If some value in the list is a two-digit Integer, then break it down.
sumDigits :: [Integer] -> Integer
sumDigits []     = 0 -- Base case for an empty list.
sumDigits (x:xs) = if x < 10 then x + sumDigits xs
                 else sumDigits ((xs) ++ (splitMultiNumberDigit(x)))
                 
-- [Bouke] I think you can do this using dropLastDigit and lastDigit instead of the helper you have created
-- I had something like this (can probably be done better)
-- sumDigits :: [Integer] -> Integer
-- sumDigits [] = 0
-- sumDigits (x:xs) = (sumDigitsSingle x) + sumDigits xs where
--   sumDigitsSingle :: Integer -> Integer
--   sumDigitsSingle y | (y>0) = (lastDigit y) + sumDigitsSingle (dropLastDigit y)
--                     | otherwise = 0


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n = length (toRevDigits n) == 16     -- Make sure there is the correct creditcard size
         && (sumDigits $ doubleEveryOther -- Obtain the sum of all digits, double the even ones from the right
           $ toRevDigits n)               -- The doubled ones are created from the reversed list
           `mod` 10 == 0                  -- Modulo 10 should be 0


-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []                               -- Base case with int = 0
hanoi 1 n1 n2 _ = [(n1, n2)]                    -- Base case where there is only 1 left shift that to peg 2
hanoi n n1 n2 n3 = hanoi(n - 1) n1 n3 n2         -- Move the A to C n1 -> n3 add n2 because we have to
                   ++ [(n1, n2)] ++              -- Add the current one (this is the current move)
                   hanoi(n - 1) n3 n2 n1         -- Move the C to B n3 -> n2 add n1 because we have to
