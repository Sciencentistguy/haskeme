{-# LANGUAGE RankNTypes #-}

module Evaluator.Procedure.Number where

import Control.Monad
import Types
import Util

lvNumericOp :: (forall a. Num a => a -> a -> a) -> LispValue -> LispValue -> Maybe LispValue
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

lvFractionalOp :: (forall a. Fractional a => a -> a -> a) -> LispValue -> LispValue -> Maybe LispValue
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

lvAdd :: [LispValue] -> LispValue
lvAdd params =
  let zero = NumberValue $ Exact $ SchemeInteger 0
      res = foldM (lvNumericOp (+)) zero params
   in case res of
        Just a -> a
        Nothing -> error "Error: operands to + must be numbers"

lvSub :: [LispValue] -> LispValue
lvSub params =
  let zero = NumberValue $ Exact $ SchemeInteger 0
      res = foldM (lvNumericOp (-)) zero params
   in case res of
        Just a -> a
        Nothing -> error "Error: operands to - must be numbers"

lvMul :: [LispValue] -> LispValue
lvMul params =
  let one = NumberValue $ Exact $ SchemeInteger 1
      res = foldM (lvNumericOp (*)) one params
   in case res of
        Just a -> a
        Nothing -> error "Error: operands to * must be numbers"

lvDiv :: [LispValue] -> LispValue
lvDiv params =
  let one = NumberValue $ Exact $ SchemeInteger 1
      res = foldM (lvFractionalOp (/)) one params
   in case res of
        Just a -> a
        Nothing -> error "Error: operands to / must be numbers"

lvMod :: [LispValue] -> LispValue
lvMod [a, b] =
  let res = do
        a <- valueToNumber a
        b <- valueToNumber b
        let exactness = case (a, b) of
              (Exact _, Exact _) -> Exact
              _ -> Inexact
        return $
          NumberValue $ exactness case (removeExactness a, removeExactness b) of
            (SchemeInteger a, SchemeInteger b) -> SchemeInteger $ a `mod` b
            _ -> unreachable
   in case res of
        Just a -> a
        Nothing -> error "Error: operands to `modulo` must be integers"
lvMod _ = error "Error: `modulo` takes two operands"

lvQuot :: [LispValue] -> LispValue
lvQuot [a, b] =
  let res = do
        a <- valueToNumber a
        b <- valueToNumber b
        let exactness = case (a, b) of
              (Exact _, Exact _) -> Exact
              _ -> Inexact
        return $
          NumberValue $ exactness case (removeExactness a, removeExactness b) of
            (SchemeInteger a, SchemeInteger b) -> SchemeInteger $ a `quot` b
            _ -> unreachable
   in case res of
        Just a -> a
        Nothing -> error "Error: operands to `quotient` must be integers"
lvQuot _ = error "Error: `quotient` takes two operands"

lvRem :: [LispValue] -> LispValue
lvRem [a, b] =
  let res = do
        a <- valueToNumber a
        b <- valueToNumber b
        let exactness = case (a, b) of
              (Exact _, Exact _) -> Exact
              _ -> Inexact
        return $
          NumberValue $ exactness case (removeExactness a, removeExactness b) of
            (SchemeInteger a, SchemeInteger b) -> SchemeInteger $ a `rem` b
            _ -> unreachable
   in case res of
        Just a -> a
        Nothing -> error "Error: operands to `remainder` must be integers"
lvRem _ = error "Error: `remainder` takes two operands"
