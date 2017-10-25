module DrawGraphTest where

import Data.Maybe (isJust, isNothing)
import Data.Set as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assert)

import DrawGraph (addNode, addEdge, merge, DepGraph) 

mainDrawGraphTest :: TestTree
mainDrawGraphTest = testGroup "Graph" [simple, complex]

simple :: TestTree
simple = testGroup "Basic tests: addNode, addEdge"
  [
      testCase "Add a node" (assert $ nodes oneNodeGraph == 1)
    , testCase "Add a second node" (assert $ nodes (addNode p2 oneNodeGraph) == 2)
    , testCase "Check nodes and edges" (assert $ nodes complexNodeGraph == 4 && edges complexNodeGraph == 3)
    , testCase "Add an existing node" (assert $ nodes (addNode p1 oneNodeGraph) == 1)
    , testCase "Add an existing edge" (assert $ edges (addEdge (p1, p2) complexNodeGraph) == 3)
  ]

complex :: TestTree
complex = testGroup "Complex tests: merge"
  [
      testCase "Add a node" (assert $
                                let g = merge (["Imp"],["Exp"]) oneNodeGraph
                                in nodes g == 3 && edges g == 1)
    , testCase "Merge complex graph" (assert $
                    let g = merge (["n1", "n2"],["exp"]) complexNodeGraph
                    in nodes g == 7 && edges g == 5)
  ]

p1 = "p1"
c1 = "c1"
p2 = "p2"
c2 = "c2"

complexNodeGraph :: DepGraph
complexNodeGraph =  addEdge (p2, c2) . addEdge (p1, p2) . addEdge (p1, c1) . addNode c2 . addNode c1 . addNode p2 $ oneNodeGraph

oneNodeGraph :: DepGraph
oneNodeGraph = addNode p1 (S.empty, S.empty)

nodes :: DepGraph -> Int
nodes (vs, es) = S.size vs

edges :: DepGraph -> Int
edges (vs, es) = S.size es
