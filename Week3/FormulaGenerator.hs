{-# OPTIONS_GHC -Wall #-}
module FormulaGenerator where

import Test.QuickCheck
import Lecture3
import Conversion
import Control.Monad
import FormClassification

{-
  I added a property called isEquivalentAfterConversion, because we want to be sure
  that there are no modifications on logical end to the formulas. The check goes ok:

  Sample output from the last lines of the tests:

  Passed:
  (*(0)==>-2)
  Passed:
  +(-2 (-1<=>3) +(2))
  Passed:
  ((1==>(2==>4))<=>(+(1)==>(-4==>3)))
  Passed:
  0
  Passed:
  (1==>0)
  Passed:
  ((1<=>2)<=>-1)
  Passed:
  (2==>+(-2))
  Passed:
  0
  +++ OK, passed 100 tests.

  The other properties are not yet working. My suspicion is that the order of
  tokens are different from what they are in natural (on paper) written formula's.
  To give an example:

  The input of convertform could be:
    ((1==>2)<=>(-2==>-1))
  And the output will be as follows:
    *(+(-1 2 1) +(-1 2 -2) +(-1 2 -2) +(-1 2 1) +(2 -1 1) +(2 -1 -2) +(2 -1 -2) +(2 -1 1))

  The properties to test with are testing on the input variant instead of the output.

  Time spent: 12 hours

-}

instance Arbitrary Form where
 arbitrary = sized form'
   where
     form' 0 = liftM Prop arbitrarySizedIntegral
     form' n | n>0 = oneof [liftM Prop arbitrarySizedIntegral, liftM Neg form'', liftM2 Impl form'' form'', liftM2 Equiv form'' form'', liftM Cnj (replicateM n form''), liftM Dsj (replicateM n form'') ]
      where form'' = form' (n `div` 2)

isConjunctionOfDisjunctions :: Form -> Bool
isConjunctionOfDisjunctions f = isConjunctionOfDisjunctions' $ lexer $ show $ convertForm f

isConjunctionOfDisjunctions' :: [Token] -> Bool
isConjunctionOfDisjunctions' []     = True
isConjunctionOfDisjunctions' (x:xs) = case x of
                                      TokenOP   {} -> isConjunctionOfDisjunctions' xs
                                      TokenInt  {} -> isConjunctionOfDisjunctions' xs
                                      TokenNeg  {} -> case head xs of
                                          TokenInt  {} -> isConjunctionOfDisjunctions' xs
                                          _ -> False
                                      TokenCnj  {} -> isConjunctionOfDisjunctions' xs
                                      TokenDsj  {} ->  case head xs of
                                          TokenInt  {} -> isConjunctionOfDisjunctions' xs
                                          TokenNeg  {} -> isConjunctionOfDisjunctions' xs
                                          _ -> False
                                      TokenCP   {} -> case head xs of
                                          TokenCnj {} -> isConjunctionOfDisjunctions' xs
                                          TokenCP  {} -> isConjunctionOfDisjunctions' xs
                                          _ -> False
                                      TokenEquiv{} -> False
                                      TokenImpl {} -> False


isDisjunctionOfLiterals :: Form -> Bool
isDisjunctionOfLiterals f = isDisjunctionOfLiterals' $ lexer $ show $ convertForm f

isDisjunctionOfLiterals' :: [Token] -> Bool
isDisjunctionOfLiterals' []     = True
isDisjunctionOfLiterals' (x:xs) = case x of
                                      TokenOP   {} -> case head xs of
                                          TokenCP  {} -> isDisjunctionOfLiterals' xs
                                          TokenDsj {} -> isDisjunctionOfLiterals' xs
                                          _ -> False
                                      _  -> isDisjunctionOfLiterals' xs

-- The following method will check if the formulas are still equivalent after the conversion
-- to cnf.
isEquivalentAfterConversion :: Form -> Bool
isEquivalentAfterConversion f = equivalence f (convertForm f)

testProp :: (Form -> Bool) -> IO()
testProp f = verboseCheckWith (stdArgs{maxSize=5}) f
