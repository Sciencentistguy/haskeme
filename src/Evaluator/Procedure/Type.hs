module Evaluator.Procedure.Type where

import Types

lvIsSymbol :: [LispValue] -> LispValue
lvIsSymbol [val] = BooleanValue case val of
  SymbolValue _ -> True
  _ -> False
lvIsSymbol _ = error "Error: procedure `symbol?` takes 1 argument."

lvIsList :: [LispValue] -> LispValue
lvIsList [val] = BooleanValue case val of
  ListValue _ -> True
  _ -> False
lvIsList _ = error "Error: procedure `list?` takes 1 argument."

lvIsDottedList :: [LispValue] -> LispValue
lvIsDottedList [val] = BooleanValue case val of
  DottedListValue _ _ -> True
  _ -> False
lvIsDottedList _ = error "Error: procedure `dotted?` takes 1 argument."

lvIsBoolean :: [LispValue] -> LispValue
lvIsBoolean [val] = BooleanValue case val of
  BooleanValue _ -> True
  _ -> False
lvIsBoolean _ = error "Error: procedure `boolean?` takes 1 argument."

lvIsCharacter :: [LispValue] -> LispValue
lvIsCharacter [val] = BooleanValue case val of
  CharacterValue _ -> True
  _ -> False
lvIsCharacter _ = error "Error: procedure `char?` takes 1 argument."

lvIsString :: [LispValue] -> LispValue
lvIsString [val] = BooleanValue case val of
  StringValue _ -> True
  _ -> False
lvIsString _ = error "Error: procedure `string?` takes 1 argument."

lvIsNumber :: [LispValue] -> LispValue
lvIsNumber [val] = BooleanValue case val of
  NumberValue _ -> True
  _ -> False
lvIsNumber _ = error "Error: procedure `number?` takes 1 argument."

lvIsVector :: [LispValue] -> LispValue
lvIsVector [val] = BooleanValue case val of
  BooleanValue _ -> True
  _ -> False
lvIsVector _ = error "Error: procedure `vector?` takes 1 argument."
