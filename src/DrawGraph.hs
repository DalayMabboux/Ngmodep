module DrawGraph (addNode, merge, get, nextInt) where

import Data.List (find)
import Data.Graph.Inductive.Graph (labEdges, labNodes, insNode, insEdge)
import Data.Maybe
import Types (NgGraph(..))

-- | Add a new node with the given string to the graph
addNode :: NgGraph -> String -> NgGraph
addNode g s = case get g s of
            Nothing -> NgGraph (insNode (ni, s) (gr g))
            Just _ -> g
            where ni = nextInt g

-- | May be return the Node fron the graph with the given label
get :: NgGraph -> String -> Maybe Int
get ng s = fst <$> find f (labNodes $ gr ng)
                      where f = (== s) . snd

getX :: NgGraph -> String -> Int
getX ng s = fromMaybe (error "asdf") (get ng s)

nextInt :: NgGraph -> Int
nextInt g = length $ labNodes $ Types.gr g
  
merge :: NgGraph -> ([String], [String]) -> NgGraph
merge ng (is, es) = foldl f ng' is
                      where
                        f a i = iE i $ addNode a i -- add the import to the graph and add then the edge
                        iE i _ng = NgGraph $ insEdge (getX _ng i, ixe, ()) $ gr _ng
                        ng' = addNode ng (head es) -- Add export node
                        (Just ixe) = get ng' (head es) -- Get exports index
