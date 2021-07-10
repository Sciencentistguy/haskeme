module Evaluator.Primitive.Type where

import Types

lvIsSymbol :: SchemeFunction
lvIsSymbol [val] = Right $ BooleanValue case val of
  SymbolValue _ -> True
  _ -> False
lvIsSymbol args = Left $ NumArgsError 1 args

lvIsList :: SchemeFunction
lvIsList [val] = Right $ BooleanValue case val of
  ListValue _ -> True
  _ -> False
lvIsList args = Left $ NumArgsError 1 args

lvIsDottedList :: SchemeFunction
lvIsDottedList [val] = Right $ BooleanValue case val of
  DottedListValue _ _ -> True
  _ -> False
lvIsDottedList args = Left $ NumArgsError 1 args

lvIsBoolean :: SchemeFunction
lvIsBoolean [val] = Right $ BooleanValue case val of
  BooleanValue _ -> True
  _ -> False
lvIsBoolean args = Left $ NumArgsError 1 args

lvIsCharacter :: SchemeFunction
lvIsCharacter [val] = Right $ BooleanValue case val of
  CharacterValue _ -> True
  _ -> False
lvIsCharacter args = Left $ NumArgsError 1 args

lvIsString :: SchemeFunction
lvIsString [val] = Right $ BooleanValue case val of
  StringValue _ -> True
  _ -> False
lvIsString args = Left $ NumArgsError 1 args

lvIsNumber :: SchemeFunction
lvIsNumber [val] = Right $ BooleanValue case val of
  NumberValue _ -> True
  _ -> False
lvIsNumber args = Left $ NumArgsError 1 args

lvIsVector :: SchemeFunction
lvIsVector [val] = Right $ BooleanValue case val of
  BooleanValue _ -> True
  _ -> False
lvIsVector args = Left $ NumArgsError 1 args
