{-# OPTIONS_GHC -Wall #-}
module Iban_2 where

hasValidLength :: String -> Bool
hasValidLength x | length x >= 4 && length x <=34 = True
                 | otherwise = False

--- Getting country codes from : http://data.okfn.org/data/core/country-list
countryCodes :: [String]
countryCodes = ["AD","AE"]

hasValidCountryCode :: String -> Bool
hasValidCountryCode x | elem x countryCodes = True
                      | otherwise = False

iban :: String -> Bool
iban x = False
