{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module Evaluator where

import Control.Monad
import Data.Maybe
import Evaluator.Procedure
import Types

eval :: LispValue -> LispValue
eval val@(StringValue _) = val
eval val@(NumberValue _) = val
eval val@(BooleanValue _) = val
eval (ListValue [AtomValue "quote", val]) = val
eval (ListValue (AtomValue func : args)) = apply func $ eval <$> args
eval _ = error "NYI"

apply fName args = case builtins fName of
  Just f -> f args
  Nothing -> error $ "Error: procedure `" ++ fName ++ "` does not exist."
