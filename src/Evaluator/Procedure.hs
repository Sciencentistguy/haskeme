module Evaluator.Procedure where

import Evaluator.Procedure.Boolean
import Evaluator.Procedure.List
import Evaluator.Procedure.Number
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
  "car" -> Just car
  "cdr" -> Just cdr
  "cons" -> Just cons
  "eq?" -> Just eqv
  "eqv?" -> Just eqv
  "equal?" -> Just eqv
  _ -> Nothing
