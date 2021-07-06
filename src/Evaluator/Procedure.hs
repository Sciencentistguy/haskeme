module Evaluator.Procedure where

import Control.Monad
import Data.Char
import Evaluator.Procedure.Boolean
import Evaluator.Procedure.List
import Evaluator.Procedure.Number
import Evaluator.Procedure.String
import Evaluator.Procedure.Symbol
import Evaluator.Procedure.Type
import Types

builtins :: String -> Maybe SchemeFunction
builtins func = case func of
  "+" -> Just $ lvNumericFoldOp (+)
  "-" -> Just $ lvNumericFoldOp (-)
  "*" -> Just $ lvNumericFoldOp (*)
  "/" -> Just $ lvFractionalFoldop (/)
  "modulo" -> Just $ lvIntegralBinop mod
  "quotient" -> Just $ lvIntegralBinop quot
  "remainder" -> Just $ lvIntegralBinop rem
  "symbol?" -> Just lvIsSymbol
  "list?" -> Just lvIsList
  "dotted?" -> Just lvIsDottedList
  "boolean?" -> Just lvIsBoolean
  "char?" -> Just lvIsCharacter
  "string?" -> Just lvIsString
  "number?" -> Just lvIsNumber
  "vector?" -> Just lvIsVector
  "symbol->string" -> Just lvSymbolToString
  "string->symbol" -> Just lvStringToSymbol
  "=" -> Just $ lvBoolBinop valueToNumber (==)
  "<" -> Just $ lvBoolBinop valueToNumber (>)
  ">" -> Just $ lvBoolBinop valueToNumber (<)
  "/=" -> Just $ lvBoolBinop valueToNumber (/=)
  ">=" -> Just $ lvBoolBinop valueToNumber (>=)
  "<=" -> Just $ lvBoolBinop valueToNumber (<=)
  "&&" -> Just $ lvBoolBinop valueToBool (&&)
  "||" -> Just $ lvBoolBinop valueToBool (||)
  "string=?" -> Just $ lvBoolBinop valueToString (==)
  "string<?" -> Just $ lvBoolBinop valueToString (>)
  "string>?" -> Just $ lvBoolBinop valueToString (<)
  "string>=?" -> Just $ lvBoolBinop valueToString (>=)
  "string<=?" -> Just $ lvBoolBinop valueToString (<=)
  "string-ci=?" -> Just $ lvBoolBinop valueToLowerString (==)
  "string-ci<?" -> Just $ lvBoolBinop valueToLowerString (>)
  "string-ci>?" -> Just $ lvBoolBinop valueToLowerString (<)
  "string>=?" -> Just $ lvBoolBinop valueToLowerString (>=)
  "string<=?" -> Just $ lvBoolBinop valueToLowerString (<=)
  "car" -> Just car
  "cdr" -> Just cdr
  "cons" -> Just cons
  "eq?" -> Just eqv
  "eqv?" -> Just eqv
  "equal?" -> Just eqv
  "make-string" -> Just makeString
  "string" -> Just string
  "string-length" -> Just stringLength
  "string-ref" -> Just stringRef
  "substring" -> Just substring
  "string-append" -> Just stringAppend
  "string->list" -> Just stringToList
  "list->string" -> Just listToString
  _ -> Nothing

valueToLowerString :: LispValue -> SchemeResult String
valueToLowerString x = do
  x <- valueToString x
  return $ toLower <$> x
