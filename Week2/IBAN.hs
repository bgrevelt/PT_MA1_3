{-# OPTIONS_GHC -Wall #-}

module IBAN where
  
import Data.Char
  
countyCodes :: [String]
countyCodes =[ "AD","AE","AF","AG","AI","AL","AM","AO","AQ","AR","AS","AT","AU","AW","AX","AZ","BA","BB","BD","BE","BF","BG","BH","BI","BJ","BL","BM","BN","BO","BQ","BR","BS","BT","BV","BW","BY","BZ","CA","CC","CD","CF","CG","CH","CI","CK","CL","CM","CN","CO","CR","CU","CV","CW","CX","CY","CZ","DE","DJ","DK","DM","DO","DZ","EC","EE","EG","EH","ER","ES","ET","FI","FJ","FK","FM","FO","FR","GA","GB","GD","GE","GF","GG","GH","GI","GL","GM","GN","GP","GQ","GR","GS","GT","GU","GW","GY","HK","HM","HN","HR","HT","HU","ID","IE","IL","IM","IN","IO","IQ","IR","IS","IT","JE","JM","JO","JP","KE","KG","KH","KI","KM","KN","KP","KR","KW","KY","KZ","LA","LB","LC","LI","LK","LR","LS","LT","LU","LV","LY","MA","MC","MD","ME","MF","MG","MH","MK","ML","MM","MN","MO","MP","MQ","MR","MS","MT","MU","MV","MW","MX","MY","MZ","NA","NC","NE","NF","NG","NI","NL","NO","NP","NR","NU","NZ","OM","PA","PE","PF","PG","PH","PK","PL","PM","PN","PR","PS","PT","PW","PY","QA","RE","RO","RS","RU","RW","SA","SB","SC","SD","SE","SG","SH","SI","SJ","SK","SL","SM","SN","SO","SR","SS","ST","SV","SX","SY","SZ","TC","TD","TF","TG","TH","TJ","TK","TL","TM","TN","TO","TR","TT","TV","TW","TZ","UA","UG","UM","US","UY","UZ","VA","VC","VE","VG","VI","VN","VU","WF","WS","YE","YT","ZA","ZM","ZW"]

-- Convert iban string to electronic form (no spaces)
toElec:: String -> String
toElec (a:b:c:d:x:xs) = if (x == ' ') then [a,b,c,d] ++ toElec xs
else [a,b,c,d, x] ++ toElec xs
toElec xs = xs

---- String validity check functions. These functions don't 
---- do any computation, they just check if the input string 
---- conforms to the specification

-- Check the length of the string. The string should be at 
-- least 4 characters (country code and checksum) and at most
-- 34 characters. (Although one could argue that a BBAN of 0
-- chars would not make much sense, it is not explicitly 
-- forbidden by the specification)
hasValidLength :: String -> Bool
hasValidLength x = length x >= 4 && length x <= 38

-- Check the country code agains the list of country codes in 
-- ISO 3166
hasValidCountryCode :: String -> Bool
hasValidCountryCode x = elem (take 2 x) countyCodes

-- Checks the textual(!) validity of the checksum. The checksum
-- may only contain digits
hasValidChecksum :: String -> Bool
hasValidChecksum x = digits (take 2 $ drop 2 x) where
  digits :: String -> Bool  
  digits [] = True
  digits (c:cs) = isDigit c && digits cs
  
-- Check the validity of the BBAN. The BBAN may contain 
-- [0-9] and [A-Z]
hasValidBBAN :: String -> Bool
hasValidBBAN x = validBBanChars (drop 4 x) where
  validBBanChars :: String -> Bool
  validBBanChars [] = True
  validBBanChars (c:cs) = (isUpper c || isDigit c) && validBBanChars cs
  
-- Check if there are no misplaced space characters
hasValidSpacing :: String -> Bool
hasValidSpacing i = not (elem ' ' (toElec i))

-- Wrapper for all the functions above. The if case is to make 
-- sure that we do not 
validForm :: String -> Bool
validForm x = (hasValidLength x) && (hasValidCountryCode x) && (hasValidChecksum x) && hasValidBBAN (toElec x) && hasValidSpacing x

---- Functions to validate checksum
-- Moves country code and checksum from start to end of string
moveFirst4ToEnd :: String -> String
moveFirst4ToEnd (a:b:c:d:xs) = xs ++ [a,b,c,d]
moveFirst4ToEnd _ = error "invalid iban!"

-- Map alpha characters to numeric values as stated in the 
-- iban specification. 
charToVal :: Char -> String
charToVal 'A' = "10";
charToVal 'B' = "11";
charToVal 'C' = "12";
charToVal 'D' = "13";
charToVal 'E' = "14";
charToVal 'F' = "15";
charToVal 'G' = "16";
charToVal 'H' = "17";
charToVal 'I' = "18";
charToVal 'J' = "19";
charToVal 'K' = "20";
charToVal 'L' = "21";
charToVal 'M' = "22";
charToVal 'N' = "23";
charToVal 'O' = "24";
charToVal 'P' = "25";
charToVal 'Q' = "26";
charToVal 'R' = "27";
charToVal 'S' = "28";
charToVal 'T' = "29";
charToVal 'U' = "30";
charToVal 'V' = "31";
charToVal 'W' = "32";
charToVal 'X' = "33";
charToVal 'Y' = "34";
charToVal 'Z' = "35";
charToVal _ = error "invalid character in iban"

-- convert an iban string to an iban string in which all alpha
-- characters have been replaced by their numeric counterparts
convertAlfaToNum :: String -> String
convertAlfaToNum [] = []
convertAlfaToNum (x:xs) = if isAlpha x then charToVal x ++ convertAlfaToNum xs else x : convertAlfaToNum xs 

-- Verify the checksum in the iban number
checkChecksum :: String -> Bool
checkChecksum [] = False
checkChecksum x = mod (read $ convertAlfaToNum $ moveFirst4ToEnd $ toElec x) 97 == 1



iban :: String -> Bool
iban [] = False 
iban x = (validForm x) && (checkChecksum x)