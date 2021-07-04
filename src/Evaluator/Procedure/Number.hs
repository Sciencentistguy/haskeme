{-# LANGUAGE RankNTypes #-}

module Evaluator.Procedure.Number where

import Control.Monad
import Types
import Util

lvNumericOp :: (forall a. Num a => a -> a -> a) -> LispValue -> LispValue -> SchemeResult LispValue
lvNumericOp f a b = do
  a <- valueToNumber a
  b <- valueToNumber b
  let exactness = case (a, b) of
        (Exact _, Exact _) -> Exact
        _ -> Inexact
  return $
    NumberValue $ exactness case (removeExactness a, removeExactness b) of
      (SchemeReal a, SchemeReal b) -> SchemeReal $ a `f` b
      (SchemeReal a, SchemeRational b) -> SchemeReal $ a `f` fromRational b
      (SchemeReal a, SchemeInteger b) -> SchemeReal $ a `f` fromInteger b
      (SchemeRational a, SchemeReal b) -> SchemeReal $ fromRational a `f` b
      (SchemeRational a, SchemeRational b) -> SchemeRational $ a `f` b
      (SchemeRational a, SchemeInteger b) -> SchemeRational $ a `f` fromInteger b
      (SchemeInteger a, SchemeReal b) -> SchemeReal $ fromInteger a `f` b
      (SchemeInteger a, SchemeRational b) -> SchemeRational $ fromInteger a `f` b
      (SchemeInteger a, SchemeInteger b) -> SchemeInteger $ a `f` b

lvFractionalOp :: (forall a. Fractional a => a -> a -> a) -> LispValue -> LispValue -> SchemeResult LispValue
lvFractionalOp f a b = do
  a <- valueToNumber a
  b <- valueToNumber b
  let exactness = case (a, b) of
        (Exact _, Exact _) -> Exact
        _ -> Inexact
  return $
    NumberValue $ exactness case (removeExactness a, removeExactness b) of
      (SchemeReal a, SchemeReal b) -> SchemeReal $ a `f` b
      (SchemeReal a, SchemeRational b) -> SchemeReal $ a `f` fromRational b
      (SchemeReal a, SchemeInteger b) -> SchemeReal $ a `f` fromInteger b
      (SchemeRational a, SchemeReal b) -> SchemeReal $ fromRational a `f` b
      (SchemeRational a, SchemeRational b) -> SchemeRational $ a `f` b
      (SchemeRational a, SchemeInteger b) -> SchemeRational $ a `f` fromInteger b
      (SchemeInteger a, SchemeReal b) -> SchemeReal $ fromInteger a `f` b
      (SchemeInteger a, SchemeRational b) -> SchemeRational $ fromInteger a `f` b
      -- TODO this should check there can't be an exact float
      (SchemeInteger a, SchemeInteger b) -> SchemeRational $ toRational a `f` toRational b

lvAdd :: SchemeFunction
lvAdd params =
  let zero = NumberValue $ Exact $ SchemeInteger 0
   in foldM (lvNumericOp (+)) zero params

lvSub :: SchemeFunction
lvSub params =
  let zero = NumberValue $ Exact $ SchemeInteger 0
   in foldM (lvNumericOp (-)) zero params

lvMul :: SchemeFunction
lvMul params =
  let one = NumberValue $ Exact $ SchemeInteger 1
   in foldM (lvNumericOp (*)) one params

lvDiv :: SchemeFunction
lvDiv params =
  let one = NumberValue $ Exact $ SchemeInteger 1
   in foldM (lvFractionalOp (/)) one params

lvMod :: SchemeFunction
lvMod [a, b] = do
  a <- valueToNumber a
  b <- valueToNumber b
  let exactness = case (a, b) of
        (Exact _, Exact _) -> Exact
        _ -> Inexact
  return $
    NumberValue $ exactness case (removeExactness a, removeExactness b) of
      (SchemeInteger a, SchemeInteger b) -> SchemeInteger $ a `mod` b
      _ -> unreachable
lvMod args = Left $ NumArgsError 2 args

lvQuot :: SchemeFunction
lvQuot [a, b] = do
  a <- valueToNumber a
  b <- valueToNumber b
  let exactness = case (a, b) of
        (Exact _, Exact _) -> Exact
        _ -> Inexact
  return $
    NumberValue $ exactness case (removeExactness a, removeExactness b) of
      (SchemeInteger a, SchemeInteger b) -> SchemeInteger $ a `quot` b
      _ -> unreachable
lvQuot args = Left $ NumArgsError 2 args

lvRem :: SchemeFunction
lvRem [a, b] = do
  a <- valueToNumber a
  b <- valueToNumber b
  let exactness = case (a, b) of
        (Exact _, Exact _) -> Exact
        _ -> Inexact
  return $
    NumberValue $ exactness case (removeExactness a, removeExactness b) of
      (SchemeInteger a, SchemeInteger b) -> SchemeInteger $ a `rem` b
      _ -> unreachable
lvRem args = Left $ NumArgsError 2 args
