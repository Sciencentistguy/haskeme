module Evaluator.Primitive.String where

import Control.Monad
import Control.Monad.Except
import Data.Char
import Data.List
import Types

makeString :: SchemeFunction
makeString [k'] = do
  k <- valueToInteger k'
  return $ StringValue $ genericTake k $ repeat '\0'
makeString [k', c'] = do
  k <- valueToInteger k'
  c <- valueToChar c'
  return $ StringValue $ genericTake k $ repeat c
makeString bal = Left $ TooManyArgsError 2 bal

string :: SchemeFunction
string args = StringValue <$> traverse valueToChar args

stringLength :: SchemeFunction
stringLength [s] = do
  s <- valueToString s
  return $ NumberValue $ Exact $ SchemeInteger $ toInteger $ length s
stringLength bal = Left $ NumArgsError 1 bal

stringRef :: SchemeFunction
stringRef [s, i] = do
  s <- valueToString s
  i <- fromInteger <$> valueToInteger i
  when (i > length s) $
    throwError $
      DefaultError $
        "Invalid index `"
          ++ show i
          ++ "` of string `"
          ++ s
          ++ "`."
  return $ CharacterValue $ s !! i
stringRef bal = Left $ NumArgsError 2 bal

substring :: SchemeFunction
substring [s, start, end] = do
  s <- valueToString s
  start <- valueToInteger start
  end <- valueToInteger end
  when (start > end) $
    throwError $
      DefaultError "The start of the substring must not be past the end of the substring."
  when (end > genericLength s) $
    throwError $
      DefaultError $
        "Invalid index `"
          ++ show end
          ++ "` of string `"
          ++ s
          ++ "`."
  let len = end - start
  return $ StringValue $ genericTake len $ genericDrop start s
substring bal = Left $ NumArgsError 3 bal

stringAppend :: SchemeFunction
stringAppend strings = do
  strings <- traverse valueToString strings
  return $ StringValue $ join strings

stringToList :: SchemeFunction
stringToList [s] = do
  s <- valueToString s
  return $ ListValue $ CharacterValue <$> s
stringToList bal = Left $ NumArgsError 1 bal

listToString :: SchemeFunction
listToString [l] = do
  l <- valueToList l
  s <- traverse valueToChar l
  return $ StringValue s
listToString bal = Left $ NumArgsError 1 bal
