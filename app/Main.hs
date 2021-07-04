module Main where

import Evaluator
import Parser
import System.Environment
import Text.Megaparsec.Error

main :: IO ()
main = do
  (expr : _) <- getArgs
  ast <- case readExpr expr of
    Right val -> return val
    Left err -> do
      putStrLn $ errorBundlePretty err
      return $ error ""
  print ast
  print $ eval ast
