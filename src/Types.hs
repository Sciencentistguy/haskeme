module Types where

import Data.Vector (Vector)
import Data.Void (Void)
import Text.Megaparsec

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

valueToNumber :: LispValue -> SchemeResult SchemeNumber
valueToNumber v = case v of
  NumberValue n -> Right n
  _ -> Left $ TypeMismatchError "number" v

valueToInteger :: LispValue -> SchemeResult Integer
valueToInteger v = case v of
  lv@(NumberValue v) -> case removeExactness v of
    SchemeInteger a -> Right a
    _ -> Left $ TypeMismatchError "integer" lv
  _ -> Left $ TypeMismatchError "number" v

removeExactness :: SchemeNumber -> SchemeNumber'
removeExactness a = case a of
  Exact a -> a
  Inexact a -> a

data SchemeNumber
  = Exact SchemeNumber'
  | Inexact SchemeNumber'
  deriving (Show, Eq)

data SchemeNumber'
  = SchemeReal Double
  | SchemeRational Rational
  | SchemeInteger Integer
  deriving (Show, Eq)

data LispError
  = NumArgsError Integer [LispValue]
  | TypeMismatchError String LispValue
  | ParserError (ParseErrorBundle String Void)
  | BadSpecialFormError String LispValue
  | NotFunctionError String String
  | UnboundVariableError String String
  | DefaultError String

instance Show LispError where
  show (NumArgsError expected found) =
    "Error: expected "
      ++ show expected
      ++ " arguments. Actually found `"
      ++ show found
      ++ "`."
  show (TypeMismatchError expected found) =
    "Error: expected type `"
      ++ expected
      ++ "`. Actually found `"
      ++ show found
      ++ "`."
  show (ParserError peb) = errorBundlePretty peb
  show (BadSpecialFormError message form) =
    "Error: bad special form `"
      ++ show form
      ++ "`."
      ++ if not $ null message
        then " (" ++ message ++ ")"
        else ""
  show (NotFunctionError message func) =
    "Error: `"
      ++ func
      ++ "` is not a function."
      ++ if not $ null message
        then " (" ++ message ++ ")"
        else ""
  show (UnboundVariableError message varname) =
    "Error: variable `"
      ++ varname
      ++ "` is unbound."
      ++ if not $ null message
        then " (" ++ message ++ ")"
        else ""
  show (DefaultError message) =
    "Error: "
      ++ message

type SchemeResult = Either LispError

type SchemeFunction = [LispValue] -> SchemeResult LispValue
