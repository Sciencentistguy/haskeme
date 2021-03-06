{-# LANGUAGE FlexibleContexts #-}

module Evaluator.Environment where

import Control.Monad.Except
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Maybe
import Types

nullEnvPtr :: IO (IORef Environment)
nullEnvPtr = newIORef HM.empty

isBound :: IORef Environment -> String -> IO Bool
isBound ptr name = do
  map <- readIORef ptr
  return $ isJust $ HM.lookup name map

getVar :: IORef Environment -> String -> IOResult LispValue
getVar ptr name = do
  env <- liftIO $ readIORef ptr
  let var = HM.lookup name env
  case var of
    Just v -> (liftIO . readIORef) v
    Nothing -> throwError $ UnboundVariableError "Getting an unbound variable" name

setVar :: IORef Environment -> String -> LispValue -> IOResult LispValue
setVar ptr name val = do
  env <- liftIO $ readIORef ptr
  let varPtr = HM.lookup name env
  case varPtr of
    Just varPtr -> liftIO (writeIORef varPtr val) >> return val
    Nothing -> throwError $ UnboundVariableError "Setting an unbound variable" name

defineVar :: IORef Environment -> String -> LispValue -> IOResult LispValue
defineVar envPtr name value = do
  alreadyDefined <- liftIO $ isBound envPtr name
  if alreadyDefined
    then setVar envPtr name value >> return value
    else liftIO do
      valuePtr <- newIORef value
      modifyIORef' envPtr $ HM.insert name valuePtr
      return value

bindVars :: IORef Environment -> [(String, LispValue)] -> IO (IORef Environment)
bindVars envPtr bindings = do
  env <- readIORef envPtr
  bindings <- traverse liftIORef bindings
  let x = extendEnv bindings env
  newIORef x
  where
    extendEnv xs env = HM.union (HM.fromList xs) env
    liftIORef (x, v) = do
      v <- newIORef v
      return (x, v)

--extendEnv ((k, v) : xs) env = do
--v <- newIORef v
--return $ extendEnv xs $ HM.insert k v env

--TODO bindvars
