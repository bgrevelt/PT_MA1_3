{-# OPTIONS_GHC -Wall #-}
module FormulaGenerator where

{-
 I am not going to create a FormulaGenerator. Instead I will make use
 of QuickCheck and come up with some testable properties.
-}

import Test.QuickCheck
import Lecture3
import Conversion

isConjunctionOfDisjunctions :: Form -> Bool
isConjunctionOfDisjunctions f = isConjunctionOfDisjunctions' $ lexer $ show $ convertForm f

isConjunctionOfDisjunctions' :: [Token] -> Bool
isConjunctionOfDisjunctions' []     = True
isConjunctionOfDisjunctions' (x:xs) = case x of
                                      TokenOP   {} -> isConjunctionOfDisjunctions' xs
                                      TokenInt  {} -> isConjunctionOfDisjunctions' xs
                                      TokenNeg  {} -> case head xs of
                                          TokenInt  {} -> isConjunctionOfDisjunctions' xs
                                          TokenOP   {} -> isConjunctionOfDisjunctions' xs
                                          _ -> False
                                      TokenCnj  {} -> isConjunctionOfDisjunctions' xs
                                      TokenDsj  {} ->  case head xs of
                                          TokenInt  {} -> isConjunctionOfDisjunctions' xs
                                          TokenNeg  {} -> isConjunctionOfDisjunctions' xs
                                          _ -> False
                                      TokenCP   {} -> case head xs of
                                          TokenCnj {} -> isConjunctionOfDisjunctions' xs
                                          TokenCP  {} -> isConjunctionOfDisjunctions' xs
                                          _           -> False
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
