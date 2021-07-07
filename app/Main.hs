module Main where

import Data.Either
import Data.Either.Combinators
import Evaluator
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
    [input] -> evalAndPrint input
    _ -> putStrLn "Too many arguments provided"

flushStr :: String -> IO ()
flushStr str = do
  putStr str
  hFlush stdout

readWithPrompt :: String -> IO String
readWithPrompt str = do
  flushStr str
  getLine

evalString :: String -> SchemeResult LispValue
evalString expr = do
  parsed <- readExpr expr
  eval parsed

evalAndPrint :: String -> IO ()
evalAndPrint expr = putStrLn $ case evalString expr of
  Right x -> case x of
    sv@(StringValue _) -> show sv
    nv@(NumberValue _) -> show nv
    cv@(CharacterValue _) -> show cv
    x -> '\'' : show x
  Left x -> show x

runRepl :: IO ()
runRepl = until_ (== "quit") (readWithPrompt "> ") evalAndPrint

until_ :: Monad m => (t -> Bool) -> m t -> (t -> m a) -> m ()
until_ p prompt action = do
  result <- prompt
  if p result
    then return ()
    else action result >> until_ p prompt action
