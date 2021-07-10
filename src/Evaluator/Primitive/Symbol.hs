module Evaluator.Primitive.Symbol where

import Types

lvSymbolToString :: SchemeFunction
lvSymbolToString [a] = case a of
  SymbolValue s -> Right $ StringValue s
  _ -> Left $ TypeMismatchError "symbol" a
lvSymbolToString args = Left $ NumArgsError 1 args

lvStringToSymbol :: SchemeFunction
lvStringToSymbol [a] = case a of
  StringValue s -> Right $ SymbolValue s
  _ -> Left $ TypeMismatchError "string" a
lvStringToSymbol args = Left $ NumArgsError 1 args
