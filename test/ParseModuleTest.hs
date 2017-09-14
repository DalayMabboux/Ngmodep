import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec
import Test.Hspec.Megaparsec

import ParseModule (parseImportsExports, ImportStmt (..), ExportStmt (..))

main :: IO ()
main = do
  c <- readFile "test/simpleModule.ts"
  h <- readFile "test/complexModule.ts"
  defaultMain (testGroup "Our Library Tests" [simple, complex, mixed, simpleModule c, complexModule h])

simple :: TestTree
simple = testGroup "Simple import / exports"
  [
    testCase "Parses import"
      (parse parseImportsExports "" "import {A}" `shouldParse` (ImportStmt ["A"], ExportStmt []))
  , testCase "Parses export"
      (parse parseImportsExports "" "export {A}" `shouldParse` (ImportStmt [], ExportStmt ["A"]))
  , testCase "Parses export with spaces"
      (parse parseImportsExports "" "export    {   A       }  " `shouldParse` (ImportStmt [], ExportStmt ["A"]))
  , testCase "Parses import and export"
      (parse parseImportsExports "" "import {A}\nexport {B}" `shouldParse` (ImportStmt ["A"], ExportStmt ["B"]))
  ]

complex :: TestTree
complex = testGroup "Complex imports"
  [
    testCase "newline"
      (parse parseImportsExports "" "import {A}\n" `shouldParse` (ImportStmt ["A"],ExportStmt []))
  , testCase "restofline with newline"
      (parse parseImportsExports "" "import {A}restofline\n" `shouldParse` (ImportStmt ["A"],ExportStmt []))
  , testCase "restofline with newline"
      (parse parseImportsExports "" "import {A}restofline\nimport   {  B}" `shouldParse` (ImportStmt ["B","A"],ExportStmt []))
  , testCase "With spaces and 'rest of line'"
      (parse parseImportsExports "" "import {  A  }  restofline  \nimport   {  B  } rest of linee  " `shouldParse` (ImportStmt ["B","A"],ExportStmt []))
  ]

mixed :: TestTree
mixed = testGroup "Mixed lines"
  [
    testCase "Non impExp line at the beginning"
      (parse parseImportsExports "" "this is a line\nimport {A}\n" `shouldParse` (ImportStmt ["A"],ExportStmt []))
  , testCase "Non impExp lines"
      (parse parseImportsExports "" "this is a line\nimport {A}basdfd\nNonSense\nexport   {B  }\blah" `shouldParse` (ImportStmt ["A"],ExportStmt ["B"]))
  , testCase "Similar words 'imporFoo' or 'exportBlah'"
      (parse parseImportsExports "" "this is a line\nimporFoo {C   }\nimport {A}basdfd\nNonSense\nexport   {B  }\nblah\nexportBlah  {D}" `shouldParse` (ImportStmt ["A"],ExportStmt ["B"]))
  ]

simpleModule :: String -> TestTree
simpleModule s = testGroup "Simple module"
  [
    testCase "Parses the content of the file correctly"
      (parse parseImportsExports "" s `shouldParse` (ImportStmt ["FormsModule","CommonModule","NgModule"],ExportStmt []))
  ]

complexModule :: String -> TestTree
complexModule s = testGroup "Complex module"
  [
    testCase "Parses the content of the file correctly"
      (parse parseImportsExports "" s `shouldParse` (ImportStmt ["FormsModule","CommonModule","NgModule"],ExportStmt ["MyComponent"]))
  ]
