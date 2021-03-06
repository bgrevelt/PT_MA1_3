module ParseTest
where
import Lecture3


{-
Test report:
  Test method: using predefined in- and output and test if the expected output matches with the output from the parse method. 
  Reason for predefined testing is that with automated generation of test cases it's very difficult to know if you test all 
  the different scenarios. Another reason for not using automated testing is that swapping some parts of valid expressions 
  to make them invalid may accidentally lead to valid cases. In such a scenario your test doesn't give back the correct result.

"Test passed on: '\"1\"'"
"Test passed on: '\"-2\"'"
"Test passed on: '\"\"'"
"Test passed on: '\"*\"'"
"Test passed on: '\"+\"'"
"Test passed on: '\"==>\"'"
"Test passed on: '\"(1==>3)\"'"
"Test passed on: '\"(1<=>3)\"'"
"Test passed on: '\"*(1 +(2 -3))\"'"
"Test passed on: '\"*(1 +(2 -3)\"'"
"Test passed on: '\" *  (  1 + ( 2 - 3 ) )\"'"
"Test passed on: '\"(*(2 -3)<=>-*(9 3))\"'"
"Test passed on: '\"(*(2 -3)==>-+(9 3))\"'"
"Done"

After the meeting on sunday the 20th, we concluded that the expected results were different
from what the method specification said it would. Therefore we changed the expected results
to include brackets, because that is what the method's output is.

Time spent: approximately 6 hours
-}


xs = [
      -- testing single value
      ("1",                       "[1]"),
      -- testing negation
      ("-2",                      "[-2]"),
      -- testing empty form
      ("",                        "[]"),
      -- testing conjunction without values
      ("*",                       "[]"),
      -- testing disjunction without values
      ("+",                       "[]"),
      -- testing implication without values
      ("==>",                     "[]"),
      -- testing implication without parenthesis
      -- ("1==>3",                   "1==>3"),
      -- testing implication with parenthesis
      ("(1==>3)",                 "[(1==>3)]"),
      -- testing  equivalence without parenthesis
      -- ("1<=>3",                  "1<=>3"),
      -- testing  equivalence with parenthesis
      ("(1<=>3)",                "[(1<=>3)]"),
      -- testing combination of negation, disjunction and conjunction
      ("*(1 +(2 -3))",            "[*(1 +(2 -3))]"),
      -- testing invalid Form, missing one bracket ')'  at the end
      ("*(1 +(2 -3)",             "[]"),
      -- testing whitespaces
      (" *  (  1 + ( 2 - 3 ) )",  "[*(1 +(2 -3))]"),
      -- testing combination of negation, disjunction, conjunction and equivalence
      ("(*(2 -3)<=>-*(9 3))",       "[(*(2 -3)<=>-*(9 3))]"),
      -- testing combination of negation, disjunction, conjunction and implication
      ("(*(2 -3)==>-+(9 3))",       "[(*(2 -3)==>-+(9 3))]")
      ]

compareForm :: String -> String -> Bool
compareForm a b = show(parse (a)) == b

testParse :: [(String,String)] -> (String -> String -> Bool) -> IO ()
testParse [] _ = do print ("Done")
testParse ((s1,s2): pair) f =
  if (f s1 s2)
    then do
      print ("Test passed on: '" ++ show s1 ++"'")
      testParse pair f
    else do
      print ("Test failed when parsing: '"++s1++"'. Expected result: '" ++ s2 ++ "', but got: '" ++ (show (parse s1)) ++ "' ")
      testParse pair f


-- Lines below give some extra info, but is not printed
ys = [(x,y) | (x,y) <- xs, compareForm x y]

numberOfTests = length xs
numberOfTestsPassed = length ys

allTestsPass = numberOfTests == numberOfTestsPassed
