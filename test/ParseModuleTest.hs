module ParseModuleTest (mainParseModuleTest) where

import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec
import Test.Hspec.Megaparsec (shouldParse)

import ParseModule (parseImportsExports)

mainParseModuleTest :: IO (TestTree)
mainParseModuleTest = do
  c <- readFile "test/simpleModule.ts"
  h <- readFile "test/complexModule.ts"
  return $ testGroup "Our Library Tests" [simple, complex, mixed, simpleModule c, complexModule h]

simple :: TestTree
simple = testGroup "Simple import / exports"
  [
    testCase "Parses import"
      (parse parseImportsExports "" "import {A}" `shouldParse` (["A"], []))
  , testCase "Parses export"
      (parse parseImportsExports "" "export {A}" `shouldParse` ([], ["A"]))
  , testCase "Parses export with spaces"
      (parse parseImportsExports "" "export    {   A       }  " `shouldParse` ([], ["A"]))
  , testCase "Parses import and export"
      (parse parseImportsExports "" "import {A}\nexport {B}" `shouldParse` (["A"], ["B"]))
  ]

complex :: TestTree
complex = testGroup "Complex imports"
  [
    testCase "newline"
      (parse parseImportsExports "" "import {A}\n" `shouldParse` (["A"], []))
  , testCase "restofline with newline"
      (parse parseImportsExports "" "import {A}restofline\n" `shouldParse` (["A"], []))
  , testCase "restofline with newline"
      (parse parseImportsExports "" "import {A}restofline\nimport   {  B}" `shouldParse` (["B","A"], []))
  , testCase "With spaces and 'rest of line'"
      (parse parseImportsExports "" "import {  A  }  restofline  \nimport   {  B  } rest of linee  " `shouldParse` (["B","A"], []))
  ]

mixed :: TestTree
mixed = testGroup "Mixed lines"
  [
    testCase "Non impExp line at the beginning"
      (parse parseImportsExports "" "this is a line\nimport {A}\n" `shouldParse` (["A"], []))
  , testCase "Non impExp lines"
      (parse parseImportsExports "" "this is a line\nimport {A}basdfd\nNonSense\nexport   {B  }\blah" `shouldParse` (["A"], ["B"]))
  , testCase "Similar words 'imporFoo' or 'exportBlah'"
      (parse parseImportsExports "" "this is a line\nimporFoo {C   }\nimport {A}basdfd\nNonSense\nexport   {B  }\nblah\nexportBlah  {D}" `shouldParse` (["A"], ["B"]))
  ]

simpleModule :: String -> TestTree
simpleModule s = testGroup "Simple module"
  [
    testCase "Parses the content of the file correctly"
      (parse parseImportsExports "" s `shouldParse` (["FormsModule","CommonModule","NgModule"], []))
  ]

complexModule :: String -> TestTree
complexModule s = testGroup "Complex module"
  [
    testCase "Parses the content of the file correctly"
      (parse parseImportsExports "" s `shouldParse` (["FormsModule","CommonModule","NgModule"], ["MyComponent"]))
  ]
