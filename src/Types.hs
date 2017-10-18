module Types (NgGraph(..)) where

import Data.Graph.Inductive.PatriciaTree (Gr)

newtype NgGraph = NgGraph { gr :: Gr String () }
  deriving (Show, Eq)