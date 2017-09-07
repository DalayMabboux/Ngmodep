module Lib (parseImportsExports, ImportStmt(..), ExportStmt(..)) where
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

data ImportStmt = ImportStmt [String]
  deriving (Show, Eq)

data ExportStmt = ExportStmt [String]
  deriving (Show, Eq)

type ImpExports = (ImportStmt, ExportStmt)

sc :: Parser ()
sc = L.space (void $ satisfy f) lineCmnt blockCmnt
  where lineCmnt = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"
        f = (== ' ')

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

importIdentifier :: Parser String
importIdentifier = lexeme $ some alphaNumChar

importIdentifierList :: Parser [String]
importIdentifierList = sepBy1 importIdentifier (symbol ",")

importExportParser :: String -> Parser [String]
importExportParser i = do
  _ <- string i *> sc
  j <- between (symbol "{") (symbol "}") importIdentifierList
  _ <- eol <|> restOfLine
  return j

impExpLine :: Parser ImpExports
impExpLine = p (ImportStmt [], ExportStmt [])
  where p s = eof *> pure s
               <|> try (importParser s >>= \a -> p a)
               <|> try (exportParser s >>= \a -> p a)
               <|> restOfLine *> pure s

restOfLine :: Parser String
restOfLine = manyTill anyChar (eol <|> eof *> return "blash")

importParser :: ImpExports -> Parser ImpExports
importParser (ImportStmt i, ExportStmt e)= importExportParser "import" >>= \s -> return $ (ImportStmt (s ++ i), ExportStmt (e))

exportParser :: ImpExports -> Parser ImpExports
exportParser (ImportStmt i, ExportStmt e)= importExportParser "export" >>= \s -> return $ (ImportStmt (i), ExportStmt (s ++ e))

-- | Returns a tuple of ImportStmt (containing a list of import statements) and ExportStmt (contains a list of export statements)
parseImportsExports :: Parser ImpExports
parseImportsExports = flatten <$> someTill impExpLine (eof *> return "eof")

flatten :: [ImpExports] -> ImpExports
flatten [] = (ImportStmt [], ExportStmt[])
flatten ies = foldr f (ImportStmt [], ExportStmt []) ies
  where
    f ((ImportStmt is, ExportStmt es)) ((ImportStmt isa, ExportStmt esa)) = (ImportStmt $ isa ++ is, ExportStmt $ esa ++ es)
