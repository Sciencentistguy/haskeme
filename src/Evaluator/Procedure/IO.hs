module Evaluator.Procedure.IO where

import Control.Monad.Except
import GHC.IO.IOMode
import Parser
import System.IO
import Types

makePort :: IOMode -> SchemeIOFunction
makePort mode [filename] = do
  filename <- liftResult $ valueToString filename
  handle <- liftIO $ openFile filename mode
  return $ IOPortValue handle
makePort _ bal = throwError $ NumArgsError 1 bal

closePort :: SchemeIOFunction
closePort [port] = do
  handle <- liftResult $ valueToHandle port
  liftIO $ hClose handle
  return $ SymbolValue "nil"
closePort bal = throwError $ NumArgsError 1 bal

read' :: SchemeIOFunction
read' [] = read' [IOPortValue stdin]
read' [port] = do
  handle <- liftResult $ valueToHandle port
  line <- liftIO $ hGetLine handle
  liftResult $ readExpr line
read' bal = throwError $ TooManyArgsError 1 bal

write' :: SchemeIOFunction
write' [obj] = write' [obj, IOPortValue stdin]
write' [obj, port] = do
  handle <- liftResult $ valueToHandle port
  liftIO $ hPrint handle obj
  return $ SymbolValue "nil"
write' bal = throwError $ TooManyArgsError 2 bal

readContents :: SchemeIOFunction
readContents [filename] = do
  filename <- liftResult $ valueToString filename
  contents <- liftIO $ readFile filename
  return $ SymbolValue contents
readContents bal = throwError $ TooManyArgsError 1 bal

load :: String -> IOResult [LispValue]
load filename = do
  file <- liftIO $ readFile filename
  liftResult $ readExprList file

readAll :: SchemeIOFunction
readAll [filename] = do
  filename <- liftResult $ valueToString filename
  ListValue <$> load filename
readAll bal = throwError $ NumArgsError 1 bal
