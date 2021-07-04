module Evaluator.Procedure where

import Evaluator.Procedure.Number
import Evaluator.Procedure.Symbol
import Evaluator.Procedure.Type
import Types

builtins :: String -> Maybe ([LispValue] -> LispValue)
builtins func = case func of
  "+" -> Just lvAdd
  "-" -> Just lvSub
  "*" -> Just lvMul
  "/" -> Just lvDiv
  "mod" -> Just lvMod
  "quotient" -> Just lvQuot
  "remainder" -> Just lvRem
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
  _ -> Nothing
