{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import Control.Monad.Except
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.Ratio
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Void (Void)
import GHC.IO.Handle
import Text.Megaparsec

type Parser = Parsec Void String

data LispValue
  = SymbolValue String
  | ListValue [LispValue]
  | DottedListValue [LispValue] LispValue
  | NumberValue SchemeNumber
  | StringValue String
  | BooleanValue Bool
  | CharacterValue Char
  | VectorValue (Vector LispValue)
  | PrimitiveFunctionValue SchemeFunction
  | FunctionValue
      { fParams :: [String],
        fVararg :: Maybe String,
        fBody :: [LispValue],
        fClosure :: IORef Environment
      }
  | IOFunctionValue SchemeIOFunction
  | IOPortValue Handle

instance Eq LispValue where
  (SymbolValue a) == (SymbolValue b) = a == b
  (ListValue a) == (ListValue b) = a == b
  (DottedListValue as a) == (DottedListValue bs b) = as == bs && a == b
  (NumberValue a) == (NumberValue b) = a == b
  (StringValue a) == (StringValue b) = a == b
  (BooleanValue a) == (BooleanValue b) = a == b
  (CharacterValue a) == (CharacterValue b) = a == b
  (VectorValue a) == (VectorValue b) = a == b
  (IOPortValue a) == (IOPortValue b) = a == b
  _ == _ = False

instance Show LispValue where
  show (SymbolValue s) = s
  show (ListValue l) = "(" ++ unwords (show <$> l) ++ ")"
  show (DottedListValue xs x) = "(" ++ unwords (show <$> xs) ++ " . " ++ show x ++ ")" --TODO
  show (NumberValue n) = show n --TODO
  show (StringValue s) = '"' : s ++ ['"']
  show (BooleanValue b) =
    if b
      then "#t"
      else "#f"
  show (CharacterValue c) =
    "\\#" ++ case c of
      ' ' -> "space"
      '\n' -> "newline"
      c -> [c]
  show (VectorValue v) = "#(" ++ unwords (V.toList $ show <$> v) ++ ")"
  show (PrimitiveFunctionValue _) = "<primitive>"
  show FunctionValue {..} =
    "(lambda (" ++ unwords (show <$> fParams)
      ++ ( case fVararg of
             Nothing -> ""
             Just arg -> " . " ++ arg
         )
      ++ ") ...)"
  show (IOPortValue x) = "<IO Port (" ++ show x ++ ")>"
  show (IOFunctionValue _) = "<IO primtive>"

valueToNumber :: LispValue -> SchemeResult SchemeNumber
valueToNumber v = case v of
  NumberValue n -> Right n
  _ -> Left $ TypeMismatchError "number" v

valueToInteger :: LispValue -> SchemeResult Integer
valueToInteger v = case v of
  lv@(NumberValue v) -> case removeExactness v of
    SchemeInteger a -> Right a
    _ -> Left $ TypeMismatchError "integer" lv
  _ -> Left $ TypeMismatchError "number" v

valueToBool :: LispValue -> SchemeResult Bool
valueToBool v = case v of
  BooleanValue False -> Right False
  _ -> Right True

valueToString :: LispValue -> SchemeResult String
valueToString v = case v of
  StringValue s -> Right s
  _ -> Left $ TypeMismatchError "string" v

valueToList :: LispValue -> SchemeResult [LispValue]
valueToList v = case v of
  ListValue l -> Right l
  _ -> Left $ TypeMismatchError "list" v

valueToChar :: LispValue -> SchemeResult Char
valueToChar v = case v of
  CharacterValue c -> Right c
  _ -> Left $ TypeMismatchError "character" v

valueToHandle :: LispValue -> SchemeResult Handle
valueToHandle v = case v of
  IOPortValue h -> Right h
  _ -> Left $ TypeMismatchError "port" v

data SchemeNumber
  = Exact SchemeNumber'
  | Inexact SchemeNumber'
  deriving (Eq)

instance Show SchemeNumber where
  show (Exact sn') = case sn' of
    SchemeInteger i -> show i
    SchemeRational rat -> show (numerator rat) ++ "/" ++ show (denominator rat)
    SchemeReal _ -> error "an exact float is impossible"
  show (Inexact sn') = case sn' of
    SchemeInteger _ -> error "an Inexact real is impssible"
    SchemeRational _ -> error "an Inexact rational is impossible"
    SchemeReal r -> show r

removeExactness :: SchemeNumber -> SchemeNumber'
removeExactness a = case a of
  Exact a -> a
  Inexact a -> a

data SchemeNumber'
  = SchemeReal Double
  | SchemeRational Rational
  | SchemeInteger Integer
  deriving (Show, Eq)

instance Ord SchemeNumber where
  compare l r = removeExactness l `compare` removeExactness r

instance Ord SchemeNumber' where
  compare l r = case (l, r) of
    (SchemeReal l, SchemeReal r) -> l `compare` r
    (SchemeReal l, SchemeRational r) -> toRational l `compare` r
    (SchemeReal l, SchemeInteger r) -> l `compare` fromInteger r
    (SchemeRational l, SchemeReal r) -> l `compare` toRational r
    (SchemeRational l, SchemeRational r) -> l `compare` r
    (SchemeRational l, SchemeInteger r) -> l `compare` toRational r
    (SchemeInteger l, SchemeInteger r) -> l `compare` r
    (SchemeInteger l, SchemeRational r) -> toRational l `compare` r
    (SchemeInteger l, SchemeReal r) -> fromInteger l `compare` r

data LispError
  = NumArgsError Integer [LispValue]
  | NotEnoughArgsError Integer [LispValue]
  | TooManyArgsError Integer [LispValue]
  | TypeMismatchError String LispValue
  | ParserError (ParseErrorBundle String Void)
  | BadSpecialFormError String LispValue
  | UnboundVariableError String String
  | DefaultError String

instance Show LispError where
  show (NumArgsError expected found) =
    "Error: expected "
      ++ show expected
      ++ " arguments. Actually found `"
      ++ show found
      ++ "`."
  show (NotEnoughArgsError min actual) =
    "Error: expected at least "
      ++ show min
      ++ " args, found `"
      ++ show actual
      ++ "`."
  show (TooManyArgsError max actual) =
    "Error: expected at most "
      ++ show max
      ++ " args, found `"
      ++ show actual
      ++ "`."
  show (TypeMismatchError expected found) =
    "Error: expected type `"
      ++ expected
      ++ "`. Actually found `"
      ++ show found
      ++ "`."
  show (ParserError peb) = errorBundlePretty peb
  show (BadSpecialFormError message form) =
    "Error: bad special form `"
      ++ show form
      ++ "`."
      ++ if not $ null message
        then " (" ++ message ++ ")"
        else ""
  show (UnboundVariableError message varname) =
    "Error: variable `"
      ++ varname
      ++ "` is unbound."
      ++ if not $ null message
        then " (" ++ message ++ ")"
        else ""
  show (DefaultError message) =
    "Error: "
      ++ message

type SchemeResult = Either LispError

type SchemeFunction = [LispValue] -> SchemeResult LispValue

type SchemeIOFunction = [LispValue] -> IOResult LispValue

type Environment = HashMap String (IORef LispValue)

type IOResult = ExceptT LispError IO

liftResult :: SchemeResult a -> IOResult a
liftResult (Left e) = throwError e
liftResult (Right v) = return v

runIOResult :: IOResult String -> IO String
runIOResult action =
  runExceptT (action `catchError` (return . show)) <&> \case
    Right x -> x
    Left x -> show x
