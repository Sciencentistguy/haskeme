{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module Evaluator where

import Control.Monad.Except
import Data.List
import Data.Maybe
import Evaluator.Procedure
import Safe
import Types

eval :: LispValue -> SchemeResult LispValue
eval val@(StringValue _) = Right val
eval val@(NumberValue _) = Right val
eval val@(BooleanValue _) = Right val
eval (ListValue [SymbolValue "quote", val]) = Right val
eval (ListValue (SymbolValue "cond" : vals)) = cond vals
eval (ListValue (SymbolValue "case" : val : clauses)) = case' val clauses
eval (ListValue [SymbolValue "if", pred, then', else']) = do
  res <- eval pred
  case res of
    BooleanValue False -> eval else'
    _ -> eval then'
eval (ListValue (SymbolValue func : args)) = do
  args <- traverse eval args
  apply func args
eval bsf = Left $ BadSpecialFormError "Not yet implemented" bsf

apply :: String -> [LispValue] -> SchemeResult LispValue
apply fName args = case builtins fName of
  Just f -> f args
  Nothing -> Left $ NotFunctionError fName "Builtin does not exist"

data CondClause
  = ArrowClause LispValue LispValue
  | StandardClause LispValue [LispValue]
  | ElseClause [LispValue]

cond :: SchemeFunction
cond clauses = do
  let parser clause = case clause of
        (SymbolValue "else" : exprs) -> Right $ ElseClause exprs
        (test : SymbolValue "=>" : [expr]) -> Right $ ArrowClause test expr
        (test : exprs) -> Right $ StandardClause test exprs
        _ -> Left $ BadSpecialFormError "`cond` clause must be of the form `(test => expr)` or `(test expr*)`" (ListValue clause)
  clauses <- traverse valueToList clauses
  clauses <- traverse parser clauses

  let doClause clause = case clause of
        ArrowClause test expr -> do
          test' <- eval test
          res <- valueToBool test'
          return
            if res
              then case eval expr of
                Left e -> Just [Left e]
                Right p -> Just [eval $ ListValue [p, test']]
              else Nothing
        StandardClause test [] -> do
          test' <- eval test
          res <- valueToBool test'
          return
            if res
              then Just [eval test']
              else Nothing
        StandardClause test exprs -> do
          res <- eval test >>= valueToBool
          return
            if res
              then Just $ eval <$> exprs
              else Nothing
        ElseClause exprs -> return $ Just $ eval <$> exprs
  parsed <- traverse doClause clauses
  let toExecute = headMay $ catMaybes parsed
  case toExecute of
    Just xs -> do
      -- execute them all, return the last
      xs <- sequenceA xs
      evaled <- traverse eval xs
      return $ last evaled
    Nothing -> return $ SymbolValue "nil" -- return value unspecified (nil)

data CaseClause
  = StandardCaseClause LispValue [LispValue]
  | ElseCaseClause [LispValue]

extractDatum :: CaseClause -> Maybe LispValue
extractDatum clause = case clause of
  StandardCaseClause d _ -> Just d
  _ -> Nothing

case' :: LispValue -> [LispValue] -> SchemeResult LispValue
case' val clauses'' = do
  let parser clause = case clause of
        (SymbolValue "else" : exprs) -> Right $ ElseCaseClause exprs
        (datum : exprs) -> Right $ StandardCaseClause datum exprs
        _ -> Left $ BadSpecialFormError "`case` clause must be of the form `(datum expr*)` or `(else expr*)`" $ ListValue clause
  clauses' <- traverse valueToList clauses''
  clauses <- traverse parser clauses'

  let datums = catMaybes $ extractDatum <$> clauses
      allUnique = nub datums == datums

  unless allUnique $ throwError $ BadSpecialFormError "all datums in a case expression must be unique" $ ListValue clauses''

  let doClause clause = case clause of
        StandardCaseClause datum exprs -> do
          datum' <- eval datum
          return
            if datum' == val
              then Just $ eval <$> exprs
              else Nothing
        ElseCaseClause exprs -> do
          return $ Just $ eval <$> exprs
  parsed <- traverse doClause clauses
  let toExecute = headMay $ catMaybes parsed
  case toExecute of
    Just xs -> do
      xs <- sequenceA xs
      evaled <- traverse eval xs
      return $ last evaled
    Nothing -> return $ SymbolValue "nil"
