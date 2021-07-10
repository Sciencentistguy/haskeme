module Main where

import Control.Monad.Except
import Data.Either
import Data.Either.Combinators
import Data.IORef
import Evaluator
import Evaluator.Environment
import Evaluator.Procedure
import Parser
import System.Environment
import System.IO
import Types
import Util

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl
    _ -> runOne args

flushStr :: String -> IO ()
flushStr str = do
  putStr str
  hFlush stdout

readWithPrompt :: String -> IO String
readWithPrompt str = do
  flushStr str
  getLine

evalString :: IORef Environment -> String -> IOResult LispValue
evalString envPtr expr = do
  parsed <- liftResult $ readExpr expr
  eval envPtr parsed

evalAndPrint :: IORef Environment -> String -> IO ()
evalAndPrint envPtr expr =
  let a = liftResult $ readExpr expr
      b = a >>= eval envPtr
      c = show <$> b
      d = runIOResult c
   in d >>= putStrLn

runOne :: [String] -> IO ()
runOne [] = unreachable
runOne args = do
  baseEnv <- primitiveBindings
  envPtr <- bindVars baseEnv [("args", ListValue $ StringValue <$> drop 1 args)]
  err <- runIOResult $ show <$> eval envPtr (ListValue [SymbolValue "load", StringValue (head args)])
  hPutStrLn stderr err

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readWithPrompt "> ") . evalAndPrint

until_ :: Monad m => (t -> Bool) -> m t -> (t -> m a) -> m ()
until_ p prompt action = do
  result <- prompt
  if p result
    then return ()
    else action result >> until_ p prompt action
