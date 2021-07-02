module Types where

import Data.Vector (Vector)
import Data.Void (Void)
import Text.Megaparsec (Parsec)

type Parser = Parsec Void String

data LispValue
  = AtomValue String
  | ListValue [LispValue]
  | DottedListValue [LispValue] LispValue
  | NumberValue SchemeNumber
  | StringValue String
  | BooleanValue Bool
  | CharacterValue Char
  | VectorValue (Vector LispValue)
  deriving (Show, Eq)

data SchemeNumber
  = Exact SchemeNumber'
  | Inexact SchemeNumber'
  deriving (Show, Eq)

data SchemeNumber'
  = SchemeComplex
      { scReal :: SchemeNumber',
        scImag :: SchemeNumber'
      }
  | SchemeReal Double
  | SchemeRational Rational
  | SchemeInteger Integer
  deriving (Show, Eq)
