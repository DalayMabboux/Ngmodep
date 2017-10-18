module ParseModule (parseImportsExports, parseModule) where

import Control.Monad.Catch (MonadThrow, Exception, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

data NgParseError = TooManyExports | ParseException String

instance Show NgParseError where
  show TooManyExports = "There has to be just one export statement"
  show (ParseException s) = "Megaparsec exception: " ++ s

instance Exception NgParseError

-- | Internal types
type Is = [String]
type Es = [String]
type IsEs = (Is, Es)

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

impExpLine :: Parser (Is,Es)
impExpLine = p ([],[])
  where p s = eof *> pure s
               <|> try (importParser s >>= \a -> p a)
               <|> try (exportParser s >>= \a -> p a)
               <|> restOfLine *> pure s

restOfLine :: Parser String
restOfLine = manyTill anyChar (eol <|> eof *> return "blash")

importParser :: IsEs -> Parser IsEs
importParser (i, e) = importExportParser "import" >>= \s -> return (s ++ i, e)

exportParser :: IsEs -> Parser IsEs
exportParser (i, e) = importExportParser "export" >>= \s -> return (i , s ++ e)

parseImportsExports :: Parser IsEs
parseImportsExports = flatten <$> someTill impExpLine (eof *> return "eof")

flatten :: [IsEs] -> IsEs
flatten []  = ([],[])
flatten ies = foldr f ([],[]) ies
 where
  f (is, e) (isa, ea) = (isa ++ is, ea ++ e)

parseModule :: (MonadThrow m, MonadIO m) => String -> m ([String],[String])
parseModule f = do
  c <- liftIO (readFile f)
  case runParser parseImportsExports "" c of
    Left e -> throwM $ ParseException (show e)
    Right t@(_, es) -> if length es /= 1 then throwM TooManyExports
                      else return t
