module ParseTest
where
import Lecture3



{-
Test report:
  Test method: using predefined in- and output and test if the expected output matches with the output from the parse method.


  Outcome of the tests by executing "testParse xs compareForm": 
  4 of the 10 tests failed. Here are all results of the tests:
  Test passed on: '\*(1 +(2 -3))\'
  Test passed on: '\*(1 +(2 -3)\'
  Test passed on: '\*(1            +(2-3))\'
  Test passed on: '\1\'
  Test passed on: '\-2\'
  Test passed on: '\\'
  Test failed when parsing: '+'. Expected result: '', but got: '[]' 
  Test failed when parsing: '==>'. Expected result: '', but got: '[]' 
  Test failed when parsing: '1==>3'. Expected result: '1==>3', but got: '[1]' 
  Test failed when parsing: '1<==>3'. Expected result: '1<==>3', but got: '[1]' 

  Time spent: approximately 5 hours
-}


xs = [
      ("*(1 +(2 -3))",            "[*(1 +(2 -3))]"),
      ("*(1 +(2 -3)",             "[]"),
      ("*(1            +(2-3))",  "[*(1 +(2 -3))]"),
      ("1",                       "[1]"),
      ("-2",                      "[-2]"),
      ("",                        "[]"),
      ("+",                       ""),
      ("==>",                     ""),
      ("1==>3",                   "1==>3"),
      ("1<==>3",                   "1<==>3")
      ]

compareForm :: String -> String -> Bool
compareForm a b = show(parse (a)) == b

ys = [(x,y) | (x,y) <- xs, compareForm x y]

numberOfTests = length xs
numberOfTestsPassed = length ys

allTestsPass = numberOfTests == numberOfTestsPassed


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


