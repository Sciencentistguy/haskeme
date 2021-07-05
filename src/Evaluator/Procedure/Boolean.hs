module Evaluator.Procedure.Boolean where

import Types

lvBoolBinop :: (LispValue -> SchemeResult a) -> (a -> a -> Bool) -> [LispValue] -> SchemeResult LispValue
lvBoolBinop unpacker op [a, b] = do
  a <- unpacker a
  b <- unpacker b
  return $ BooleanValue $ a `op` b
lvBoolBinop _ _ args = Left $ NumArgsError 2 args

eqv :: SchemeFunction
eqv [a, b] = case (a, b) of
  (BooleanValue a, BooleanValue b) -> Right $ BooleanValue $ a == b
  (NumberValue a, NumberValue b) -> Right $ BooleanValue $ a == b
  (SymbolValue a, SymbolValue b) -> Right $ BooleanValue $ a == b
  (StringValue a, StringValue b) -> Right $ BooleanValue $ a == b
  (DottedListValue as a, DottedListValue bs b) -> eqv [ListValue $ as ++ [a], ListValue $ bs ++ [b]]
  (ListValue a, ListValue b) -> Right $ BooleanValue $ (length a == length b) && all eqvPair (zip a b)
    where
      eqvPair (x1, x2) = case eqv [x1, x2] of
        Right (BooleanValue v) -> v
        _ -> False
  (_, _) -> Right $ BooleanValue False
eqv bal = Left $ NumArgsError 2 bal
