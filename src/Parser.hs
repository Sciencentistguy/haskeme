module Parser where

import Control.Monad
import Data.Char
import Data.Either.Combinators
import qualified Data.Vector as V
import Data.Void (Void)
import Parser.Number
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Types

spaces :: Parser ()
spaces =
  L.space
    space1
    (L.skipLineComment ";")
    empty

symbol :: String -> Parser String
symbol = L.symbol spaces

pStringLit :: Parser LispValue
pStringLit = do
  let delimiter = char '"'
  _ <- delimiter
  str <- manyTill L.charLiteral delimiter
  return $ StringValue str

pLispSymbol :: Parser Char
pLispSymbol = choice $ char <$> "!#$%&|*+-/:<=>?@^_~"

pSymbol :: Parser LispValue
pSymbol = do
  first <- letterChar <|> pLispSymbol
  rest <- many $ alphaNumChar <|> pLispSymbol
  let atom = first : rest
  return case atom of
    "#t" -> BooleanValue True
    "#f" -> BooleanValue False
    _ -> SymbolValue atom

pCharLit :: Parser LispValue
pCharLit = do
  _ <- string "#\\"
  c <- anySingle
  more <- many letterChar
  let name =
        if isAlpha c && not (null more)
          then Just $ c : more
          else Nothing
  case name of
    Nothing -> return $ CharacterValue c
    Just "space" -> return $ CharacterValue ' '
    Just "newline" -> return $ CharacterValue '\n'
    -- TODO are there more of these?
    _ -> fail "invalid named character in character literal"

pList :: Parser LispValue
pList = ListValue <$> sepEndBy pExpr spaces

pDottedList :: Parser LispValue
pDottedList = do
  head <- endBy pExpr spaces
  tail <- do
    _ <- char '.'
    spaces
    pExpr
  return $ DottedListValue head tail

pQuoted :: Parser LispValue
pQuoted = do
  _ <- char '\''
  x <- pExpr
  return $ ListValue [SymbolValue "quote", x]

pVector :: Parser LispValue
pVector = do
  _ <- symbol "#("
  vec <- sepBy pExpr spaces
  _ <- symbol ")"
  return $ VectorValue $ V.fromList vec

pQuasiQuoteExpr :: Parser LispValue
pQuasiQuoteExpr = do
  c <- optional $ char ','
  e <- pExpr
  return case c of
    Just _ -> ListValue [SymbolValue "unquote", e]
    Nothing -> e

pQuasiQuoteList :: Parser LispValue
pQuasiQuoteList = do
  _ <- symbol "("
  l <- sepBy pQuasiQuoteExpr spaces
  _ <- symbol ")"
  return $ ListValue l

pQuasiQuote :: Parser LispValue
pQuasiQuote = do
  _ <- char '`'
  x <- pQuasiQuoteList <|> pQuasiQuoteExpr
  return $ ListValue [SymbolValue "quasiquote", x]

pExpr :: Parser LispValue
pExpr =
  pVector
    <|> pNumberLit
    <|> pCharLit
    <|> pQuasiQuote
    <|> pQuoted
    <|> pSymbol
    <|> pStringLit
    <|> do
      _ <- symbol "("
      x <- try pList <|> pDottedList
      _ <- symbol ")"
      return x

readExpr :: String -> Either LispError LispValue
readExpr x = mapLeft ParserError $ parse (spaces >> pExpr) "<stdin>" x

assert :: MonadFail m => Bool -> String -> m ()
assert expr msg = unless expr $ fail $ "assert failed: " ++ msg
