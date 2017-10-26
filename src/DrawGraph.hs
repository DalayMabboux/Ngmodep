module DrawGraph (addNode, merge, addEdge, addNode, DepGraph, morphToDot, pushOut) where

import Data.List (find)
import Data.GraphViz (graphToDot, nonClusteredParams, graphToDot)
import Data.GraphViz (GraphvizParams(..), defaultParams, fmtNode, toLabel, graphElemsToDot, printDotGraph)
import Data.GraphViz.Printing (renderDot, toDot)
import Data.GraphViz.Commands.IO (writeDotFile)
import Data.Maybe (fromMaybe)
import Data.Set as S
import Data.Text.Lazy.IO as TL
import Data.Text.Internal.Lazy as T

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

morphToDot :: DepGraph -> T.Text
morphToDot (vs, es) = printDotGraph $ graphElemsToDot params vs' es'
                        where vs' = fmap (\a -> (a,a)) $ S.toList vs
                              es' = fmap (\(a,b) -> (a,b,())) $ S.toList es

params :: GraphvizParams n String el () String
params = defaultParams {
  fmtNode = \(_,l) -> [toLabel l]
}

pushOut :: T.Text -> IO ()
pushOut = TL.writeFile "test.dot"

                         