module ParseModule (parseImportsExports, parseModule) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadThrow)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Types (ImportStmt(..), ExportStmt(..), ImpExports(..))


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
impExpLine = p empty
  where p s = eof *> pure s
               <|> try (importParser s >>= \a -> p a)
               <|> try (exportParser s >>= \a -> p a)
               <|> restOfLine *> pure s

restOfLine :: Parser String
restOfLine = manyTill anyChar (eol <|> eof *> return "blash")

importParser :: ImpExports -> Parser ImpExports
importParser (ImpExports (ImportStmt i, e)) = importExportParser "import" >>= \s -> return $ ImpExports (ImportStmt (s ++ i), e)

-- | TODO: handle case when there are more is more than one export statement
exportParser :: ImpExports -> Parser ImpExports
exportParser (ImpExports (ImportStmt i, e))= importExportParser "export" >>= \s -> return $ ImpExports (ImportStmt (i), ExportStmt $ Just $ head s)

-- | Returns a tuple of ImportStmt (containing a list of import statements) and ExportStmt (contains a list of export statements)
parseImportsExports :: Parser ImpExports
parseImportsExports = flatten <$> someTill impExpLine (eof *> return "eof")

flatten :: [ImpExports] -> ImpExports
flatten [] = empty
flatten ies = foldr f empty ies
  where
    e' e ea = case (e, ea) of
                (Nothing, Nothing) -> ExportStmt Nothing
                (Nothing, Just _) -> ExportStmt ea
                (Just _, Nothing) -> ExportStmt e
                (Just _, Just _) -> undefined
    f (ImpExports (ImportStmt is, ExportStmt e)) (ImpExports (ImportStmt isa, ExportStmt ea)) = ImpExports (ImportStmt $ isa ++ is, e' e ea)

parseModule :: (MonadThrow m, MonadIO m) => String -> m ImpExports
parseModule f = do
  c <- liftIO $ readFile f
  case runParser parseImportsExports "" c of
    Left _ -> return empty
    Right a -> return a
