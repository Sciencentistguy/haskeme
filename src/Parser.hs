{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Control.Monad
import Control.Monad.Except
import Data.Char
import qualified Data.Vector as V
import Data.Void (Void)
import Parser.Number
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug
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
pList = spaces >> symbol "(" >> pList1

pList1 :: Parser LispValue
pList1 =
  try do
    spaces
    _ <- symbol ")"
    return $ ListValue []
    <|> do
      expr <- pExpr
      pList2 [expr]

pList2 :: [LispValue] -> Parser LispValue
pList2 expr =
  try do
    spaces
    _ <- symbol ")"
    return $ ListValue $ reverse expr
    <|> (spaces >> pList3 expr)

pList3 :: [LispValue] -> Parser LispValue
pList3 expr =
  do
    spaces
    _ <- symbol "."
    dotted <- pExpr
    spaces
    _ <- symbol ")"
    return $ DottedListValue expr dotted
    <|> do
      next <- pExpr
      pList2 (next : expr)

pDottedList :: Parser LispValue
pDottedList = dbg "pDottedList" $ do
  init <- endBy pExpr spaces
  _ <- char '.'
  _ <- spaces
  DottedListValue init <$> pExpr

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
    <|> pList

{-
 -    do
 -      _ <- symbol "("
 -      x <- try pList <|> pDottedList
 -      _ <- symbol ")"
 -      return x
 -
 -}
readOrThrow :: MonadError LispError m => Parsec Void String a -> String -> m a
readOrThrow parser input = case parse parser "<stdin>" input of
  Left err -> throwError $ ParserError err
  Right val -> return val

readExpr :: String -> Either LispError LispValue
readExpr = readOrThrow pExpr

readExprList :: MonadError LispError m => String -> m [LispValue]
readExprList = readOrThrow $ endBy pExpr spaces

assert :: MonadFail m => Bool -> String -> m ()
assert expr msg = unless expr $ fail $ "assert failed: " ++ msg
