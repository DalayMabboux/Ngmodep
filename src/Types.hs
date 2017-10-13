module Types (NgGraph(..), ImportStmt(..), ExportStmt(..), ImpExports(..)) where

import Data.Graph.Inductive.PatriciaTree (Gr)

newtype NgGraph = NgGraph { gr :: Gr String () }
  deriving (Show, Eq)

newtype ImportStmt = ImportStmt [String]
  deriving (Show, Eq)

data ExportStmt = ExportStmt String
  deriving (Show, Eq)

newtype ImpExports = ImpExports (ImportStmt, ExportStmt)
  deriving (Show, Eq)
