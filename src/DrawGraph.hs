module DrawGraph (addNode, merge, addEdge, DepGraph, renderToDot) where

import Data.GraphViz (GraphvizParams(..), defaultParams, fmtNode, toLabel, graphElemsToDot, printDotGraph)
import Data.Set as S
import Data.Text.Lazy.IO as TL

type V = String
type E = (String, String)
type DepGraph = (S.Set V, S.Set E)

-- | Add a new node with the given string to the graph
addNode :: String -> DepGraph -> DepGraph
addNode s (vs, es) = (S.insert s vs, es)

addEdge :: (String, String) -> DepGraph -> DepGraph
addEdge e (vs, es) = (vs, S.insert e es)

merge :: ([String], [String]) -> DepGraph-> DepGraph
merge (is, es) ng = Prelude.foldl f ng' is
                       where
                         f a i = addEdge (i, Prelude.head es) $ addNode i a
                         ng' = addNode (Prelude.head es) ng

renderToDot :: String -> DepGraph -> IO ()
renderToDot f (vs, es) = TL.writeFile f $ printDotGraph $ graphElemsToDot params vs' es'
                        where vs' = fmap (\a -> (a,a)) $ S.toList vs
                              es' = fmap (\(a,b) -> (a,b,())) $ S.toList es

params :: GraphvizParams n String el () String
params = defaultParams {
  fmtNode = \(_,l) -> [toLabel l]
}