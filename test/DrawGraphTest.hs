module DrawGraphTest (mainDrawGraphTest) where

import Data.Graph.Inductive.Graph (empty, size, insNode, LNode(..), noNodes)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Maybe (isJust, isNothing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assert)
import Types (NgGraph(..))

import DrawGraph (addNode, nextInt, get, merge) 

mainDrawGraphTest :: TestTree
mainDrawGraphTest = testGroup "Graph" [simple]

simple :: TestTree
simple = testGroup "Basic tests"
  [
      testCase "Next int from empty graph is 0" (assert $ nextInt (NgGraph empty) == 0)
    , testCase "Next int from one graph node is 1" (assert $ nextInt (NgGraph oneNodeGraph) == 1)
    , testCase "Find a node" (assert $ isJust $ get (NgGraph oneNodeGraph) "test")
    , testCase "Dont find a node" (assert $ isNothing $ get (NgGraph oneNodeGraph) "notPresent")
    , testCase "Add a node" (assert $ noNodes (gr (addNode (NgGraph empty) "NewNode")) == 1)
    , testCase "Merge graph: 2 nodes" (assert $ noNodes (gr (merge (NgGraph empty) (["Imp"],["Exp"]))) == 2)
    , testCase "Merge graph: 1 edge" (assert $ size (gr (merge (NgGraph empty) (["Imp"],["Exp"]))) == 1)
  ]

oneNodeGraph :: Gr String ()
oneNodeGraph = insNode (0, "test") empty

