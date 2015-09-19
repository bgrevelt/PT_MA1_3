module FormClassification
where  
import Lecture3
import Data.List

{--
description of your method of checking the definitions:
  We are testing the definitions against pre defined lists or which we
  know if the functions (or function pairs) do or do not belong to a
  specific classification. 
  We could not think of a way to implement a test for this using 
  automated test case generation
indication of time spent.
  4 Hrs
--}

-- The classification functions
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

contradiction :: Form -> Bool
contradiction f = not $ satisfiable f
  
logicalEntailment :: Form -> Form -> Bool
logicalEntailment f g = tautology (Impl f g)

equivalence :: Form -> Form -> Bool
equivalence f g = tautology (Equiv f g)

-- Some pre-defined formula we can use to test our classification algorithms
equivalences :: [(Form,Form)]
equivalences = [
  -- (p => q) <=> -p || q
  ((Impl p q), (Dsj [(Neg p), q])), 
  -- (p <=> q) <=> (p&&q) || (-p && -q) 
  ((Equiv p q), (Dsj [(Cnj [p,q]),(Cnj  [(Neg p),(Neg q)])])),
  -- p && q <=> -(-p || -q)
  ((Dsj [p,q]), (Neg(Cnj [(Neg p), (Neg q)]))),
  -- (p <=> q) <=> (q <=> p)
  ((Equiv p q), (Equiv q p)),
   -- p || (q && r) <=> (p || q) && (p || r)
  ((Dsj [p, Cnj [q,r]]), (Cnj [(Dsj [p,q]),(Dsj [p,r])])),
    -- (p -> q) -> (-q -> -p)
  ((Impl p q), (Impl (Neg q) (Neg p)))
  ]
  
entailments :: [(Form,Form)]
entailments = [ 
  -- p&&q -> p||q
  ((Cnj [p,q]), (Dsj [p,q])),
  -- p && q -> (p -> q)
  ((Cnj [p,q]), (Impl p q))
  ] ++ equivalences -- All equivalences are also entailments
  
tautologies :: [Form]
tautologies = [ 
  -- p || -p
  (Dsj [ p, (Neg p) ]), 
  -- p -> p || q
  (Impl p (Dsj [p,q])),
  -- (p -> q) || (r || -q)
  (Dsj [(Impl p q), (Dsj [r, (Neg q)])])]
  -- And we can use the pairs of formula we defined earlier to create
  -- tautologies
  ++ [(Equiv a b) | (a,b) <- equivalences]
  ++ [(Impl a b) | (a,b) <- entailments]
  
contradictions :: [Form]
contradictions = [
  -- p && -p
  (Cnj [p, (Neg p)]),
  --  (p->q) && (p && -q)
  (Cnj [(Impl p q), (Cnj [p, (Neg q)])])
  ] 
  -- And we can of course easily generate contradictions from
  -- our list of tautologies using negation
  ++ [(Neg a) | a <- tautologies]

nonEquivalences :: [(Form,Form)]
nonEquivalences = [
  -- p -> q neq -p -> -q
  ((Impl p q), (Impl (Neg p) (Neg q))),
  -- (p || q) && r neq p || (q && r)
  ((Cnj [(Dsj [p,q]), r]), (Dsj [p, (Cnj [q,r])]))
  ] ++ nonEntailments -- All nonEntailments are also nonEquivalences
  -- And generate some more by negating one of the formulas in a pair  that are equivalent
  ++ [((Neg a), b) | (a,b) <- equivalences]
  ++ [((Neg b), b) | (a,b) <- equivalences]
  

  
nonEntailments :: [(Form,Form)]
nonEntailments = [
  -- p || q -/-> p -> q
  ((Dsj [p,q]), (Impl p q)),
  -- p -> q -/-> p || q
  ((Impl p q), (Dsj [p,q]))
  ]
  -- and again we can complement this be negating part of formulas pairs that are entailments
  ++ [(a, (Neg b)) | (a,b) <- entailments]
  


-- Non-tautologies are easily created from the negative lists we already have
nonTautology :: [Form]
nonTautology = 
  [(Impl a b) | (a,b) <- nonEntailments] ++
  [(Equiv a b) | (a,b) <- nonEquivalences] ++
   contradictions
  

-- The test functions 

-- One takes lists of formulas, a classfication function for a single 
-- formula and a boolean for the expected outcome of applying the function
-- to the formula 
testForm :: [(Form)] -> (Form -> Bool) -> Bool -> IO ()
testForm [] _ _ = do print ("Done")
testForm (frm: fs) f e = 
  if (f frm == e) 
    then do 
      print ("Test passed on" ++ show frm)
      testForm fs f e
    else error ("failed test on: " ++ show frm)

-- One takes pairs of formulas, a classification function for  two 
-- formulas and an expected outcome of applyin the function to the two 
-- formulas
testFormRel :: [(Form,Form)] -> (Form -> Form -> Bool) -> Bool -> IO ()
testFormRel [] _ _ = do print ("Done")
testFormRel ((frm1,frm2): frmPairs) f e = 
  if (f frm1 frm2 == e) 
    then do 
      print ("Test passed on" ++ show frm1 ++ show frm2)
      testFormRel frmPairs f e
    else error ("failed test on: " ++ show frm1 ++ show frm2)


-- Finally, a small helper function to wrap all the tests in    
test :: IO ()
test = do
  testFormRel equivalences equivalence True
  testFormRel nonEquivalences equivalence False
  testFormRel entailments logicalEntailment True
  testFormRel nonEntailments logicalEntailment False
  testForm tautologies tautology True
  testForm nonTautology tautology False
  testForm contradictions contradiction True
  testForm tautologies contradiction False