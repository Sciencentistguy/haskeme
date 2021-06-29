{-# LANGUAGE BlockArguments #-}

module Parser.Number where

import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char.Number
import Types

data NumericalTag
  = BaseTag NumericalBase
  | ExactnessTag Exactness

data NumericalBase = Decimal | Hexadecimal | Binary | Octal

data Exactness = Exact' | Inexact'

pNumericalTags :: Parser (NumericalBase, Maybe Exactness)
pNumericalTags = do
  (baseMaybe, exactness) <- do
    -- TODO make this produce better error messages
    hash <- fmap isNothing <$> optional $ char '#'
    if hash
      then return (Nothing, Nothing)
      else do
        firstTagChar <- oneOf "boxie"
        let firstTag = charToNumTag firstTagChar
        hash <- fmap isNothing <$> optional $ char '#'
        if hash
          then return case firstTag of
            BaseTag x -> (Just x, Nothing)
            ExactnessTag x -> (Nothing, Just x)
          else do
            secondTagChar <- case firstTag of
              BaseTag _ -> oneOf "ie"
              ExactnessTag _ -> oneOf "box"
            let secondTag = charToNumTag secondTagChar
            return case (firstTag, secondTag) of
              (BaseTag b, ExactnessTag e) -> (Just b, Just e)
              (ExactnessTag e, BaseTag b) -> (Just b, Just e)
              _ -> error "unreachable"
  let base = fromMaybe Decimal baseMaybe
  return (base, exactness)
  where
    charToNumTag x = case x of
      'b' -> BaseTag Binary
      'o' -> BaseTag Octal
      'x' -> BaseTag Hexadecimal
      'i' -> ExactnessTag Inexact'
      'e' -> ExactnessTag Exact'
      _ -> error "unreachable"

pIntegerLit :: Parser LispValue
pIntegerLit = do
  (base, exactness') <- pNumericalTags
  let exactness = case exactness' of
        Just Inexact' -> Inexact
        _ -> Exact
  let pInteger = case base of
        Decimal -> L.decimal
        Binary -> L.binary
        Hexadecimal -> L.hexadecimal
        Octal -> L.octal
  NumberValue . exactness . SchemeInteger <$> pInteger

pFloatingLit :: Parser LispValue
pFloatingLit = do
  (base, exactness') <- pNumericalTags
  let exactness = fromMaybe Inexact' exactness'
  let fractionParser = case base of
        Decimal -> decFloat True
        Binary -> binFloat True
        Hexadecimal -> hexFloat True
        Octal -> octFloat True
  let f x = do
        case x of
          Right f -> return f
          Left i -> fail ""
  let pFractional = fractionParser >>= f
  d <- pFractional
  case exactness of
    Inexact' -> return $ NumberValue $ Inexact $ SchemeReal d
    Exact' -> error "Rational nyi"

pNumberLit :: Parser LispValue
pNumberLit = try pFloatingLit <|> pIntegerLit
