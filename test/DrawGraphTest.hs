module DrawGraphTest (mainDrawGraphTest) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, assert)


mainDrawGraphTest :: TestTree
mainDrawGraphTest = testGroup "Graph" [simple]

simple :: TestTree
simple = testGroup "First test group"
  [
    testCase "First test"
      (assert True)
  ]
