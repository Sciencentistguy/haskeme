{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module Evaluator where

import Evaluator.Procedure
import Types

eval :: LispValue -> SchemeResult LispValue
eval val@(StringValue _) = Right val
eval val@(NumberValue _) = Right val
eval val@(BooleanValue _) = Right val
eval (ListValue [SymbolValue "quote", val]) = Right val
eval (ListValue (SymbolValue func : args)) = do
  args <- traverse eval args
  apply func args
eval bsf = Left $ BadSpecialFormError "Not yet implemented" bsf

apply :: String -> [LispValue] -> SchemeResult LispValue
apply fName args = case builtins fName of
  Just f -> f args
  Nothing -> error $ "Error: procedure `" ++ fName ++ "` does not exist."
