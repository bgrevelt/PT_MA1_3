-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

testToRevDigits:: (Integer, [Integer]) -> Bool
testToRevDigits (n, d) = toRevDigits n == d

ex2Tests :: [Test]
ex2Tests = [ Test "toRevDigits test" testToRevDigits
             [(123, [3, 2, 1]), (0, []), (-1, []), (5, [5]),
               (3219873218, [8, 1, 2, 3, 7, 8, 9, 1, 2, 3])
             ]
           ]

-- Exercise 3 -----------------------------------------

testDoubleEveryOther:: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (n, d) = doubleEveryOther n == d

ex3Tests :: [Test]
ex3Tests = [ Test "doubleEveryOther test" testDoubleEveryOther
             [([1, 2, 3, 4], [1, 4, 3, 8]), ([4, 9, 5, 5], [4, 18, 5, 10]),
              ([1, 2, 3], [1, 4, 3]), ([], []), ([1], [1])
             ]
           ]

-- Exercise 4 -----------------------------------------

testSumDigits:: ([Integer], Integer) -> Bool
testSumDigits (n, d) = sumDigits n == d

ex4Tests :: [Test]
ex4Tests = [ Test "testSumDigits test" testSumDigits
             [  ([ 1, 2, 10, 1], 5), ([11, 2, 3], 7), ([10, 5, 18, 4], 19)]
           ]

-- Exercise 5 -----------------------------------------

testLuhn:: (Integer, Bool) -> Bool
testLuhn (n, d) = luhn n == d

ex5Tests :: [Test]
ex5Tests = [ Test "luhn test" testLuhn
             [(5594589764218858, True), (1234567898765432, False),
              (12345678, False), (1234567898765432112, False),
              (55, False)]
           ]

-- Exercise 6 -----------------------------------------

--testHanoi:: (Integer, Peg, Peg, Peg, Move) -> Bool
--testHanoi (n, n1, n2, n3, d) = hanoi n n1 n2 n3 == d

ex6Tests :: [Test]
ex6Tests = []
--         [ Test "hanoi test" testHanoi
--           [(2, "a", "b", "c", True), (1, "a", "b", "c", True),
--            (0, "a", "b", "c", True), (10, "a", "b", "c", False)
--           ]
--         ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]

-- Run all the tests ----------------------------------

runAllTests :: [Failure]
runAllTests = runTests allTests
