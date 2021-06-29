module Types where

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
  deriving (Show, Eq)

data SchemeNumber
  = Exact SchemeNumber'
  | Inexact SchemeNumber'
  deriving (Show, Eq)

data SchemeNumber'
  = SchemeComplex
      { scReal :: Integer,
        scImag :: Integer
      }
  | SchemeReal Double
  | SchemeRational Rational
  | SchemeInteger Integer
  deriving (Show, Eq)
