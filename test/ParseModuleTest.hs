import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec
import Test.Hspec.Megaparsec

import ParseModule (parseImportsExports)
import Types (ImpExports(..), ImportStmt(..), ExportStmt(..))

main :: IO ()
main = do
  c <- readFile "test/simpleModule.ts"
  h <- readFile "test/complexModule.ts"
  defaultMain (testGroup "Our Library Tests" [simple, complex, mixed, simpleModule c, complexModule h])

simple :: TestTree
simple = testGroup "Simple import / exports"
  [
    testCase "Parses import"
      (parse parseImportsExports "" "import {A}" `shouldParse` ImpExports (ImportStmt ["A"], ExportStmt Nothing))
  , testCase "Parses export"
      (parse parseImportsExports "" "export {A}" `shouldParse` ImpExports (ImportStmt [], ExportStmt $ Just "A"))
  , testCase "Parses export with spaces"
      (parse parseImportsExports "" "export    {   A       }  " `shouldParse` ImpExports (ImportStmt [], ExportStmt $ Just "A"))
  , testCase "Parses import and export"
      (parse parseImportsExports "" "import {A}\nexport {B}" `shouldParse` ImpExports (ImportStmt ["A"], ExportStmt $ Just "B"))
  ]

complex :: TestTree
complex = testGroup "Complex imports"
  [
    testCase "newline"
      (parse parseImportsExports "" "import {A}\n" `shouldParse` ImpExports (ImportStmt ["A"], ExportStmt Nothing))
  , testCase "restofline with newline"
      (parse parseImportsExports "" "import {A}restofline\n" `shouldParse` ImpExports (ImportStmt ["A"], ExportStmt Nothing))
  , testCase "restofline with newline"
      (parse parseImportsExports "" "import {A}restofline\nimport   {  B}" `shouldParse` ImpExports (ImportStmt ["B","A"], ExportStmt Nothing))
  , testCase "With spaces and 'rest of line'"
      (parse parseImportsExports "" "import {  A  }  restofline  \nimport   {  B  } rest of linee  " `shouldParse` ImpExports (ImportStmt ["B","A"], ExportStmt Nothing))
  ]

mixed :: TestTree
mixed = testGroup "Mixed lines"
  [
    testCase "Non impExp line at the beginning"
      (parse parseImportsExports "" "this is a line\nimport {A}\n" `shouldParse` ImpExports (ImportStmt ["A"], ExportStmt Nothing))
  , testCase "Non impExp lines"
      (parse parseImportsExports "" "this is a line\nimport {A}basdfd\nNonSense\nexport   {B  }\blah" `shouldParse` ImpExports (ImportStmt ["A"], ExportStmt $ Just "B"))
  , testCase "Similar words 'imporFoo' or 'exportBlah'"
      (parse parseImportsExports "" "this is a line\nimporFoo {C   }\nimport {A}basdfd\nNonSense\nexport   {B  }\nblah\nexportBlah  {D}" `shouldParse` ImpExports (ImportStmt ["A"], ExportStmt $ Just "B"))
  ]

simpleModule :: String -> TestTree
simpleModule s = testGroup "Simple module"
  [
    testCase "Parses the content of the file correctly"
      (parse parseImportsExports "" s `shouldParse` ImpExports (ImportStmt ["FormsModule","CommonModule","NgModule"], ExportStmt Nothing))
  ]

complexModule :: String -> TestTree
complexModule s = testGroup "Complex module"
  [
    testCase "Parses the content of the file correctly"
      (parse parseImportsExports "" s `shouldParse` ImpExports (ImportStmt ["FormsModule","CommonModule","NgModule"], ExportStmt $ Just "MyComponent"))
  ]
