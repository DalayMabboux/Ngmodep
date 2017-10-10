module Types (NgGraph(..), ImportStmt(..), ExportStmt(..), ImpExports(..), empty) where

import Data.Graph.Inductive.PatriciaTree (Gr)

newtype NgGraph = NgGraph { gr :: Gr String () }
  deriving (Show, Eq)

newtype ImportStmt = ImportStmt [String]
  deriving (Show, Eq)

newtype ExportStmt = ExportStmt (Maybe String)
  deriving (Show, Eq)

newtype ImpExports = ImpExports (ImportStmt, ExportStmt)
  deriving (Show, Eq)

empty :: ImpExports
empty = ImpExports (ImportStmt [], ExportStmt Nothing)
