{-# OPTIONS_GHC -Wall #-}
module TestIban 
where

{-- Test report
By calling test with the number of tests to be run as the argument x tests on the iban algorithm will be executed.
Each test will randomly be of a valid or invalid iban. If an invalid iban is used, the type of invalidation 
(invalidation of checksum, bban or country code) will also be determined at random. 
Time spent: Approx. 10 Hrs (the majority of time spent was spent on getting stuff to compile with IO)
--} 
  
import IBAN
import TestFunctions
import Data.Char

-- Stupid wrapper over the function below which is a bit more generic than
-- what we really need

testIban :: Int -> IO()
testIban n = test 1 n iban

-- The actual test function. 
-- Aguments:  Current test case number 
--            Last testcase number
--            Function to test 
-- In each loop the function randomly tests either a valid or invalid iban                          
test :: Int -> Int -> (String -> Bool) -> IO()
test k n f = if k == n then print (show n ++ " tests passed")
                else do
                  v <- getRandom [True, False]
                  i <- getIban v
                  if f i == v then
                    do print ("pass on: " ++ show i)
                       test (k+1) n f
                  else error ("failed test on: " ++ show i)
                  
-- Get a random element from a supplied set
getRandom :: [a] -> IO a
getRandom cs = do 
  i <- getRandomInt((length cs) -1)
  return (cs!!i)
  
-- Get a list of random elements from a supplied set with a supplied length
getRandomList :: [a] -> Int -> IO [a]
getRandomList _ 0 = do
  return []
getRandomList cs n = do
  c <- getRandom cs
  s <- getRandomList cs (n-1)
  return (c:s)

-- Set of characters valid in an Iban  
validIbanChars :: [Char]
validIbanChars = (['0'..'9']++['A'..'Z'])

-- Set of characters that may not be part of an IBAN
invalidIbanChars :: [Char]
invalidIbanChars = filter (not.isDigit) $ filter (not.isUpper) ['!'..'~']

-- Helper function for checksum calculation, convert int to two digit string
toDDString :: Integer -> String
toDDString cs | (cs >= 100) = error "Cant convert " ++ (show cs) ++ " to two digit string"
              | otherwise = if (cs < 10) then "0" ++ show cs else show cs

-- Returns a random valid IBAN country code from the list of country codes
getRandCC :: IO String
getRandCC = do
  cc <- getRandom countyCodes
  return cc

-- Returns a random, valid BBAN   
getRandBBAN :: IO String
getRandBBAN = do 
  len <- getRandomInt 34
  str <- (getRandomList validIbanChars len)
  return str

-- Add a checksum to an IBAN string.    
addChecksum :: String -> String
addChecksum i = (take 2 i) ++ cc i ++ (drop 4 i) where
  cc :: String -> String
  cc ib = toDDString (toInteger(98 - mod (read $ convertAlfaToNum $ moveFirst4ToEnd $ toElec ib) 97))
    
-- When a valid iban is put in, an (otherwise valid) iban with an invalid checksum is returned.
mangleChecksum :: String -> IO String
mangleChecksum (cc1:cc2:cs1:cs2:is) = do
  offset <- (getRandomInt 95) 
  return (cc1:cc2:(mangle (cs1:[cs2]) (offset + 1))++is) where
    mangle :: String -> Int -> String
    mangle i o = toDDString (toInteger((mod ((read i) + o) 97))) where
mangleChecksum _ = error "invalid IBAN"

-- When a valid iban is put in, an (otherwise valid) iban with an invalid BBAN is returned.
mangleBBan :: String -> IO String
mangleBBan i = do
  nrOfMangles <- getRandomInt 34
  ret <- (mangleBBanChars i (nrOfMangles +1))
  return ret where
    mangleBBanChars :: String -> Int -> IO String
    mangleBBanChars i 0 = do return i
    mangleBBanChars i n = do
      inx <- (getRandomInt ((length i) - 5))
      invChar <- (getRandom invalidIbanChars)
      recurse <- (mangleBBanChars ((take (inx + 4) i) ++ [invChar] ++ (drop (inx+5) i)) (n-1))
      return recurse

-- When a valid iban is put in, an (otherwise valid) iban with an invalid country code is returned.      
mangleCC :: String -> IO String
mangleCC i = do
  mangledCC <- getRandInvCC
  return ((take 2 i) ++ mangledCC ++ (drop 4 i)) where
    getRandInvCC :: IO String
    getRandInvCC = do
      cc <- getRandomList ['A'..'Z'] 2
      if elem cc countyCodes then 
        do 
          cc <- getRandInvCC
          return cc
      else return cc

-- Return a random, valid, iban      
getRandValidIban :: IO String
getRandValidIban = do
  cc <- getRandCC
  bban <- getRandBBAN
  return (addChecksum (cc ++ "00" ++ bban))

-- Get a random invalid IBAN  
getRandFaultyIban ::IO String
getRandFaultyIban = do
  wayOfMangling <- getRandomInt 2
  ib <- getRandValidIban
  mib <- (mangleIban wayOfMangling ib)
  return mib  
  where
    mangleIban 0 i = mangleCC i
    mangleIban 1 i = mangleChecksum i
    mangleIban 2 i = mangleBBan i
    mangleIban _ _ = error "Invalid mangle type"
    --mangleIban _ i = mangleChecksum i

-- Wrapper around the two functions above, to get either a valid or invalid
-- Iban    
getIban :: Bool -> IO String
getIban True = getRandValidIban
getIban False = getRandFaultyIban
