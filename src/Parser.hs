{-# LANGUAGE BlockArguments #-}

module Parser where

--( readExpr,
--)

import Control.Monad
import Data.Char
import Data.Maybe
import Data.Void (Void)
import Safe
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Types

type Parser = Parsec Void String

data NumericalTag
  = BaseTag (Parser Integer)
  | ExactnessTag (SchemeNumber' -> SchemeNumber)

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

pIntegerLit :: Parser LispValue
pIntegerLit = do
  (base, exactness) <- pNumericalTags
  NumberValue . exactness . SchemeInteger <$> base

pFloatingLit :: Parser LispValue
pFloatingLit = do
  NumberValue . (Exact . SchemeReal) <$> L.float

pNumericalTags :: Parser (Parser Integer, SchemeNumber' -> SchemeNumber)
pNumericalTags = do
  (baseMaybe, exactnessMaybe) <- do
    -- TODO make this produce better error messages
    hash <- fmap isNothing <$> optional $ char '#'
    if hash
      then return (Nothing, Nothing)
      else do
        firstTagChar <- oneOf "boxie"
        let firstTag = case firstTagChar of
              'b' -> BaseTag L.binary
              'o' -> BaseTag L.octal
              'x' -> BaseTag L.hexadecimal
              'i' -> ExactnessTag Inexact
              'e' -> ExactnessTag Exact
              _ -> error "unreachable"
        hash <- fmap isNothing <$> optional $ char '#'
        if hash
          then return case firstTag of
            BaseTag x -> (Just x, Nothing)
            ExactnessTag x -> (Nothing, Just x)
          else do
            secondTagChar <- case firstTag of
              BaseTag _ -> oneOf "ie"
              ExactnessTag _ -> oneOf "box"
            let secondTag = case secondTagChar of
                  'b' -> BaseTag L.binary
                  'o' -> BaseTag L.octal
                  'x' -> BaseTag L.hexadecimal
                  'i' -> ExactnessTag Inexact
                  'e' -> ExactnessTag Exact
                  _ -> error "unreachable"
            return case (firstTag, secondTag) of
              (BaseTag b, ExactnessTag e) -> (Just b, Just e)
              (ExactnessTag e, BaseTag b) -> (Just b, Just e)
              _ -> error "unreachable"
  let base = fromMaybe L.decimal baseMaybe
  let exactness = fromMaybe Exact exactnessMaybe
  return (base, exactness)

--TODO Parser part 1 Excercise 6: floating point numbers (binary, octal, hex)
--TODO Parser part 1 Excercise 6: hierarchy of number types
--TODO Parser part 2 Excercise 1: quasiquotes
--TODO Parser part 2 Excercise 2: Vector
pExpr :: Parser LispValue
pExpr =
  try pFloatingLit
    <|> pIntegerLit
    <|> pCharLit
    <|> pAtom
    <|> pStringLit
    <|> do
      _ <- symbol "("
      x <- try pList <|> pDottedList
      _ <- symbol ")"
      return x

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

readExpr input = case parse (spaceConsumer >> pExpr) "<stdin>" input of
  Left err -> "No match:\n" ++ errorBundlePretty err
  Right val -> "Found value: " ++ show val

assert :: MonadFail m => Bool -> String -> m ()
assert expr msg = unless expr $ fail $ "assert failed: " ++ msg
