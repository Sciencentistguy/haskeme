{-# LANGUAGE BlockArguments #-}

module Parser where

import Control.Monad
import Data.Char
import Data.Maybe
import Parser.Number
import Safe
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char.Number hiding (Parser)
import Types

spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    space1
    (L.skipLineComment ";")
    empty

symbol = L.symbol spaceConsumer

pStringLit :: Parser LispValue
pStringLit = do
  let delimiter = char '"'
  _ <- delimiter
  str <- manyTill L.charLiteral delimiter
  return $ StringValue str

pLispSymbol :: Parser Char
pLispSymbol = do
  c <- choice $ symbol . (: []) <$> "!#$%&|*+-/:<=>?@^_~"
  assert (length c == 1) $ "length c == " ++ show (length c) -- TODO probably unnecessary
  return $ head c

pAtom :: Parser LispValue
pAtom = do
  first <- letterChar <|> pLispSymbol
  rest <- many $ alphaNumChar <|> pLispSymbol
  let atom = first : rest
  return case atom of
    "#t" -> BooleanValue True
    "#f" -> BooleanValue False
    _ -> AtomValue atom

pCharLit :: Parser LispValue
pCharLit = do
  _ <- string "#\\"
  c <- anySingle
  more <- many anySingle
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
pList = ListValue <$> sepBy pExpr spaceConsumer

pDottedList :: Parser LispValue
pDottedList = do
  head <- endBy pExpr spaceConsumer
  tail <- do
    _ <- char '.'
    spaceConsumer
    pExpr
  return $ DottedListValue head tail

pQuoted :: Parser LispValue
pQuoted = do
  _ <- char '\''
  x <- pExpr
  return $ ListValue [AtomValue "quote", x]

-- TODO Parser part 1 Excercise 6: hierarchy of number types
-- TODO Parser part 2 Excercise 1: quasiquotes
-- TODO Parser part 2 Excercise 2: Vector
pExpr :: Parser LispValue
pExpr =
  pNumberLit
    <|> pCharLit
    <|> pAtom
    <|> pStringLit
    <|> do
      _ <- symbol "("
      x <- try pList <|> pDottedList
      _ <- symbol ")"
      return x

readExpr input = case parse (spaceConsumer >> pExpr) "<stdin>" input of
  Left err -> "No match:\n" ++ errorBundlePretty err
  Right val -> "Found value: " ++ show val

assert :: MonadFail m => Bool -> String -> m ()
assert expr msg = unless expr $ fail $ "assert failed: " ++ msg
