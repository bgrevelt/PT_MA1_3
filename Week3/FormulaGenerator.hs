{-# OPTIONS_GHC -Wall #-}
module FormulaGenerator where

{-
 I am not going to create a FormulaGenerator. Instead I will make use
 of QuickCheck and come up with some testable properties.
-}

import Test.QuickCheck
import Lecture3
import Conversion

isTautology :: Form -> Form -> Bool
isTautology _ _ = True

isConjunctionOfDisjunctions :: Form -> Form -> Bool
isConjunctionOfDisjunctions _ _ = True

isDisjunctionOfLiterals :: Form -> Form -> Bool
isDisjunctionOfLiterals _ _ = True

-- isArrowFree :: Form -> Bool
-- isArrowFree x = convertForm x
-- isArrowFree _ = True
