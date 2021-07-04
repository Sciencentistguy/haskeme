module Evaluator.Procedure.Symbol where

import Types

lvSymbolToString :: [LispValue] -> LispValue
lvSymbolToString [a] = case a of
  SymbolValue s -> StringValue s
  _ -> error "Error: argument to `symbol->string` must be a symbol"
lvSymbolToString _ = error "Error: `symbol->string` takes 1 argument"

lvStringToSymbol :: [LispValue] -> LispValue
lvStringToSymbol [a] = case a of
  StringValue s -> SymbolValue s
  _ -> error "Error: argument to `string->symbol` must be a symbol"
lvStringToSymbol _ = error "Error: `string->symbol` takes 1 argument"
