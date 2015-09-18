{-# OPTIONS_GHC -Wall #-}
module FormulaGenerator where

{-
 I am not going to create a FormulaGenerator. Instead I will make use
 of QuickCheck and come up with some testable properties.
-}

import Test.QuickCheck
import Lecture3
import Conversion

prop_isTautology :: Form -> Form -> Bool
prop_isTautology _ _ = True

prop_isConjunctionOfDisjunctions :: Form -> Form -> Bool
prop_isConjunctionOfDisjunctions _ _ = True

prop_isDisjunctionOfLiterals :: Form -> Form -> Bool
prop_isDisjunctionOfLiterals _ _ = True


--- Sample usage with output and a maxdepth of 5 for the formula's
--- verboseCheckWith (stdArgs{maxSize=5}) isTautology
--- verboseCheckWith (stdArgs{maxSize=5}) isConjuctionOfLiterals
--- verboseCheckWith (stdArgs{maxSize=5}) isDisjunctionOfLiterals
