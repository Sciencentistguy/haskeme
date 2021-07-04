{-# LANGUAGE RankNTypes #-}

module Evaluator.Procedure.Number where

import Control.Monad
import Types
import Util

lvNumericBinop :: (forall a. Num a => a -> a -> a) -> LispValue -> LispValue -> SchemeResult LispValue
lvNumericBinop f a b = do
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

lvFractionalBinop :: (forall a. Fractional a => a -> a -> a) -> LispValue -> LispValue -> SchemeResult LispValue
lvFractionalBinop f a b = do
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

lvNumericFoldOp :: (forall a. Num a => a -> a -> a) -> [LispValue] -> Either LispError LispValue
lvNumericFoldOp op (first : rest) = foldM (lvNumericBinop op) first rest
lvNumericFoldOp _ _ = Left $ NotEnoughArgsError 1 0

lvFractionalFoldop ::
  (forall a. Fractional a => a -> a -> a) ->
  [LispValue] ->
  Either LispError LispValue
lvFractionalFoldop op (first : rest) = foldM (lvFractionalBinop op) first rest
lvFractionalFoldop _ _ = Left $ NotEnoughArgsError 1 0

lvIntegralBinop :: (Integer -> Integer -> Integer) -> [LispValue] -> Either LispError LispValue
lvIntegralBinop op [a, b] = do
  a <- valueToNumber a
  b <- valueToNumber b
  let exactness = case (a, b) of
        (Exact _, Exact _) -> Exact
        _ -> Inexact
  return $
    NumberValue $ exactness case (removeExactness a, removeExactness b) of
      (SchemeInteger a, SchemeInteger b) -> SchemeInteger $ a `op` b
      _ -> unreachable
lvIntegralBinop _ a = Left $ NumArgsError 2 a
