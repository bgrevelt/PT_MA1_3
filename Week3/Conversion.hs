module Conversion where

import Lecture3
import Data.List

--
-- Time spent: 7 hours
--
-- Essentially this but then half of it: https://april.eecs.umich.edu/courses/eecs492_w10/wiki/images/6/6b/CNF_conversion.pdf
-- Half of it because the other half was provided.

-- Move negation inward
-- Remove the arrows
-- Distribute ^ and v (This gets us doubles) and fix it to remove the wrong constructs (^^^vvv etc)
-- simplify the whole thing
convertForm :: Form -> Form
convertForm f = simpleForm $ fix $ toCNF $ nnf $ arrowfree f	


--
-- Functions that will convert to CNF (that looks really crappy)
--
toCNF :: Form -> Form                                           -- Function that converts to a sloppy CNF format
toCNF (Dsj [n]) = toCNF n                                       -- Single disjunction so return that (probably for the next call) 	
toCNF (Dsj (n1 : n2 : xs)) = reorder                            -- Take 2 disjunctions to check if there is a conjuction between
  (toCNF n1) (toCNF (Dsj(n2 : xs)))                             -- We call toCNF again to make sure it calls [n]
toCNF n = n

reorder :: Form -> Form -> Form
reorder (Cnj [n]) x      = reorder n x                          -- 1 element with some second element, reorder that
reorder n (Cnj [x])      = reorder n x                          -- 1 element with some first element, reorder that
reorder (Cnj (n : xs)) x =                                      -- For the form each element n, start swapping n and x
  Cnj [reorder n x, reorder (Cnj xs) x]		
reorder n (Cnj (x : xs)) =                                      -- Same also recheck the form Cnj 
  Cnj [reorder n x, reorder n (Cnj xs)]                         -- Create a conjuction of the set Form n x, Form n xs
reorder n x              = Dsj [n, x]                           -- If both are disjunctions this is good since this is allowed


--
-- Fix the way the disjunctions and conjuctions are ordered (this is the repeated step)
--
fix :: Form -> Form
fix (Cnj []) = Cnj []                                           -- Base case
fix (Cnj x) = Cnj (fixConjuction x)                             -- If conjuction then fix that
fix (Dsj []) = Dsj []                                           -- Base case
-- This is not a typo vvvvvvvvvv 
fix (Dsj x) = Dsj (fixConjuction x)                             -- Same but then for Disjunction
fix x = x                                                       -- Don't touch anything else (properties etc)


-- TODO: This is really ugly I would love this to be 1 function but it doesn't go that well with having to call it recursively
-- TODO: There can be things inside these so that's what makes it a bit more complicated...
fixConjuction :: [Form] -> [Form]
fixConjuction [] = []                                           -- Return if empty list base case 
fixConjuction ((Cnj x) : xs) = fixConjuction (x ++ xs)          -- for each conjuction in the form we add it to the list
fixConjuction (x : xs) = fix x : fixConjuction xs               -- for everything else add the current result to the list

fixDisjunction :: [Form] -> [Form]										
fixDisjunction [] = []                                          -- Base case
fixDisjunction ((Dsj x) : xs) = fixDisjunction (x ++ xs)        -- for each form in the thing just add it to the list
fixDisjunction (x : xs) = fix x : fixDisjunction xs             -- When we did all that then add every other element to the fix list


--
-- Simplify - Kicks out the remaining dupe vars
--
simpleForm :: Form -> Form                                      -- the repeat part of the distrilaw
simpleForm (Dsj n) = Dsj (nub n)                                -- If it is a disjunction remove the dupes from N
simpleForm (Cnj n) = Cnj (map simpleForm n)                     -- If it is a conjuction remove the disjunction that are dupes
simpleForm n       = n                                          -- Everything else is fine 
