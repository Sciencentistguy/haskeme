module Types where

import Data.Vector (Vector)
import Data.Void (Void)
import Safe
import Text.Megaparsec (Parsec)

type Parser = Parsec Void String

data LispValue
  = SymbolValue String
  | ListValue [LispValue]
  | DottedListValue [LispValue] LispValue
  | NumberValue SchemeNumber
  | StringValue String
  | BooleanValue Bool
  | CharacterValue Char
  | VectorValue (Vector LispValue)
  deriving (Show, Eq)

valueToNumber :: LispValue -> Maybe SchemeNumber
valueToNumber v = case v of
  NumberValue n -> Just n
  _ -> Nothing

valueToInteger v = case v of
  NumberValue v -> case removeExactness v of
    SchemeInteger a -> Just a
    _ -> Nothing
  _ -> Nothing

removeExactness a = case a of
  Exact a -> a
  Inexact a -> a

data SchemeNumber
  = Exact SchemeNumber'
  | Inexact SchemeNumber'
  deriving (Show, Eq)

data SchemeNumber'
  = -- SchemeComplex
    --{ scReal :: SchemeNumber',
    --scImag :: SchemeNumber'
    --} |
    SchemeReal Double
  | SchemeRational Rational
  | SchemeInteger Integer
  deriving (Show, Eq)
