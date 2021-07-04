module Main where

import Data.Either
import Evaluator
import Parser
import System.Environment
import Text.Megaparsec.Error
import Util

main :: IO ()
main = do
  (expr : _) <- getArgs
  ast <- case readExpr expr of
    Right val -> return val
    Left err -> do
      putStrLn $ errorBundlePretty err
      return $ error ""
  print ast
  let evaled = show <$> eval ast
  let v = trapError evaled
  let x = fromRight undefined v
  putStrLn x
