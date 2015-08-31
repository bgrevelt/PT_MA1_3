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


-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (n1 : n2 : ns) = n1 : 2 * n2 : doubleEveryOther ns -- Take 2 integers, multiply the second, continue
doubleEveryOther rest = rest  -- If there are 0/1 left return this.


-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = undefined                     -- sum . concatMap toRevDigits (used this for testing 5)


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
hanoi = undefined


