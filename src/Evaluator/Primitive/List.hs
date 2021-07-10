module Evaluator.Primitive.List where

import Types

car :: SchemeFunction
car [x] = case x of
  ListValue (x : _) -> Right x
  DottedListValue (x : _) _ -> Right x
  badArg -> Left $ TypeMismatchError "pair" badArg
car x = Left $ NumArgsError 1 x

cdr :: SchemeFunction
cdr [x] = case x of
  ListValue (_ : xs) -> Right $ ListValue xs
  DottedListValue [_] x -> Right x
  DottedListValue (_ : xs) x -> Right $ DottedListValue xs x
  badArg -> Left $ TypeMismatchError "pair" badArg
cdr x = Left $ NumArgsError 1 x

cons :: SchemeFunction
cons [x1, ListValue []] = Right $ ListValue [x1]
cons [x, ListValue xs] = Right $ ListValue $ x : xs
cons [x, DottedListValue xs xlast] = Right $ DottedListValue (x : xs) xlast
cons [x1, x2] = Right $ DottedListValue [x1] x2
cons x = Left $ NumArgsError 2 x
