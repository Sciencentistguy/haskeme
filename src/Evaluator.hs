{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module Evaluator where

import Control.Monad.Except
import Data.IORef
import Data.List
import Data.Maybe
import Evaluator.Environment
import Evaluator.Procedure
import Safe
import Types

eval :: IORef Environment -> LispValue -> IOResult LispValue
eval _ val@(StringValue _) = return val
eval _ val@(NumberValue _) = return val
eval _ val@(BooleanValue _) = return val
eval _ val@(CharacterValue _) = return val
eval envPtr (SymbolValue name) = getVar envPtr name
eval _ (ListValue [SymbolValue "quote", val]) = return val
eval envPtr (ListValue (SymbolValue "cond" : vals)) = cond envPtr vals
eval envPtr (ListValue (SymbolValue "case" : val : clauses)) = case' envPtr val clauses
eval envPtr (ListValue [SymbolValue "if", pred, then', else']) = do
  res <- eval envPtr pred
  case res of
    BooleanValue False -> eval envPtr else'
    _ -> eval envPtr then'
eval envPtr (ListValue (SymbolValue func : args)) = do
  args <- traverse (eval envPtr) args
  apply func args
eval _ bsf = throwError $ BadSpecialFormError "Not yet implemented" bsf

apply :: String -> [LispValue] -> IOResult LispValue
apply fName args = case builtins fName of
  Just f -> liftResult $ f args
  Nothing -> throwError $ NotFunctionError fName "Builtin does not exist"

data CondClause
  = ArrowClause LispValue LispValue
  | StandardClause LispValue [LispValue]
  | ElseClause [LispValue]

cond :: IORef Environment -> SchemeIOFunction
cond envPtr clauses = do
  let parser clause = case clause of
        (SymbolValue "else" : exprs) -> Right $ ElseClause exprs
        (test : SymbolValue "=>" : [expr]) -> Right $ ArrowClause test expr
        (test : exprs) -> Right $ StandardClause test exprs
        _ -> Left $ BadSpecialFormError "`cond` clause must be of the form `(test => expr)` or `(test expr*)`" (ListValue clause)
  clauses <- liftResult $ traverse valueToList clauses
  clauses <- liftResult $ traverse parser clauses

  let doClause clause = case clause of
        ArrowClause test expr -> do
          test' <- eval envPtr test
          res <- liftResult $ valueToBool test'
          return
            if res
              then
                let evaledExprs =
                      do
                        v <- eval envPtr expr
                        return [eval envPtr $ ListValue [v, test']]
                        `catchError` \e -> return [throwError e]
                 in Just evaledExprs
              else Nothing
        StandardClause test [] -> do
          test' <- eval envPtr test
          res <- liftResult $ valueToBool test'
          return
            if res
              then Just $ return [eval envPtr test']
              else Nothing
        StandardClause test exprs -> do
          test' <- eval envPtr test
          res <- liftResult $ valueToBool test'
          return
            if res
              then
                let a = eval envPtr <$> exprs
                 in Just $ return a
              else Nothing
        ElseClause exprs -> return $ Just $ return $ eval envPtr <$> exprs
  parsed <- traverse doClause clauses
  let toExecute = headMay $ catMaybes parsed
  case toExecute of
    Just xs -> do
      -- execute them all, return the last
      xs <- sequenceA =<< xs
      evaled <- traverse (eval envPtr) xs
      return $ last evaled
    Nothing -> return $ SymbolValue "nil" -- return value unspecified (nil)

data CaseClause
  = StandardCaseClause LispValue [LispValue]
  | ElseCaseClause [LispValue]

extractDatum :: CaseClause -> Maybe LispValue
extractDatum clause = case clause of
  StandardCaseClause d _ -> Just d
  _ -> Nothing

case' :: IORef Environment -> LispValue -> [LispValue] -> IOResult LispValue
case' envPtr val clauses' = do
  let parser clause = case clause of
        (SymbolValue "else" : exprs) -> Right $ ElseCaseClause exprs
        (datum : exprs) -> Right $ StandardCaseClause datum exprs
        _ -> Left $ BadSpecialFormError "`case` clause must be of the form `(datum expr*)` or `(else expr*)`" $ ListValue clause
  clauses <- liftResult $ traverse valueToList clauses' >>= traverse parser

  let datums = catMaybes $ extractDatum <$> clauses
      allUnique = nub datums == datums

  unless allUnique $ throwError $ BadSpecialFormError "all datums in a case expression must be unique" $ ListValue clauses'

  let doClause clause = case clause of
        StandardCaseClause datum exprs -> do
          datum' <- eval envPtr datum
          return
            if datum' == val
              then Just $ eval envPtr <$> exprs
              else Nothing
        ElseCaseClause exprs -> do
          return $ Just $ eval envPtr <$> exprs
  parsed <- traverse doClause clauses
  let toExecute = headMay $ catMaybes parsed
  case toExecute of
    Just xs -> do
      xs <- sequenceA xs
      evaled <- traverse (eval envPtr) xs
      return $ last evaled
    Nothing -> return $ SymbolValue "nil"
