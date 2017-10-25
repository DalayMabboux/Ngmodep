module DrawGraph (addNode, merge, addEdge, addNode, DepGraph) where

import Data.List (find)
import Data.GraphViz (graphToDot, nonClusteredParams, graphToDot)
import Data.GraphViz.Printing (renderDot, toDot)
import Data.GraphViz
import Data.Maybe (fromMaybe)
import Data.Set as S

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
                         f a i = addEdge (i, head es) $ addNode i a                                                                     
                         ng' = addNode (head es) ng 

-- ngGrapToDot :: NgGraph -> String
-- ngGrapToDot ng = showDot (fglToDot (gr ng))

-- writeDot :: String -> String -> IO ()
-- writeDot p s = writeFile p s
                         
                         