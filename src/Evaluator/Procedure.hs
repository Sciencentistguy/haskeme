module Evaluator.Procedure where

import Control.Monad
import Data.Char
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Evaluator.Environment
import Evaluator.Procedure.Boolean
import Evaluator.Procedure.IO
import Evaluator.Procedure.List
import Evaluator.Procedure.Number
import Evaluator.Procedure.String
import Evaluator.Procedure.Symbol
import Evaluator.Procedure.Type
import GHC.IO.IOMode
import Types

primitives :: [(String, SchemeFunction)]
primitives =
  [ ("+", lvNumericFoldOp (+)),
    ("-", lvNumericFoldOp (-)),
    ("*", lvNumericFoldOp (*)),
    ("/", lvFractionalFoldop (/)),
    ("modulo", lvIntegralBinop mod),
    ("quotient", lvIntegralBinop quot),
    ("remainder", lvIntegralBinop rem),
    ("symbol?", lvIsSymbol),
    ("list?", lvIsList),
    ("dotted?", lvIsDottedList),
    ("boolean?", lvIsBoolean),
    ("char?", lvIsCharacter),
    ("string?", lvIsString),
    ("number?", lvIsNumber),
    ("vector?", lvIsVector),
    ("symbol->string", lvSymbolToString),
    ("string->symbol", lvStringToSymbol),
    ("=", lvBoolBinop valueToNumber (==)),
    ("<", lvBoolBinop valueToNumber (>)),
    (">", lvBoolBinop valueToNumber (<)),
    ("/=", lvBoolBinop valueToNumber (/=)),
    (">=", lvBoolBinop valueToNumber (>=)),
    ("<=", lvBoolBinop valueToNumber (<=)),
    ("&&", lvBoolBinop valueToBool (&&)),
    ("||", lvBoolBinop valueToBool (||)),
    ("string=?", lvBoolBinop valueToString (==)),
    ("string<?", lvBoolBinop valueToString (>)),
    ("string>?", lvBoolBinop valueToString (<)),
    ("string>=?", lvBoolBinop valueToString (>=)),
    ("string<=?", lvBoolBinop valueToString (<=)),
    ("string-ci=?", lvBoolBinop valueToLowerString (==)),
    ("string-ci<?", lvBoolBinop valueToLowerString (>)),
    ("string-ci>?", lvBoolBinop valueToLowerString (<)),
    ("string>=?", lvBoolBinop valueToLowerString (>=)),
    ("string<=?", lvBoolBinop valueToLowerString (<=)),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eq?", eqv),
    ("eqv?", eqv),
    ("equal?", eqv),
    ("make-string", makeString),
    ("string", string),
    ("string-length", stringLength),
    ("string-ref", stringRef),
    ("substring", substring),
    ("string-append", stringAppend),
    ("string->list", stringToList),
    ("list->string", listToString)
  ]

ioPrimitives :: [(String, SchemeIOFunction)]
ioPrimitives =
  [ --("apply", applyProc),
    ("open-input-file", makePort ReadMode),
    ("open-output-file", makePort WriteMode),
    ("close-input-port", closePort),
    ("close-output-port", closePort),
    ("read", read'),
    ("write", write'),
    ("read-contents", readContents),
    ("read-all", readAll)
  ]

valueToLowerString :: LispValue -> SchemeResult String
valueToLowerString x = do
  x <- valueToString x
  return $ toLower <$> x

primitiveBindings :: IO (IORef Environment)
primitiveBindings = do
  let
  envPtr <- nullEnvPtr
  let prims = makeFunc PrimitiveFunctionValue <$> primitives
      ioPrims = makeFunc IOFunctionValue <$> ioPrimitives
  bindVars envPtr $ prims ++ ioPrims
  where
    makeFunc constructor (var, fName) = (var, constructor fName)

makeFunc ::
  (Monad m, Show a) =>
  Maybe String ->
  IORef Environment ->
  [a] ->
  [LispValue] ->
  m LispValue
makeFunc varargs envPtr params body =
  return $
    FunctionValue (show <$> params) varargs body envPtr

makeNormalFunc :: (Monad m, Show a) => IORef Environment -> [a] -> [LispValue] -> m LispValue
makeNormalFunc = makeFunc Nothing

makeVarArgs ::
  (Monad m, Show a, Show b) =>
  a ->
  IORef Environment ->
  [b] ->
  [LispValue] ->
  m LispValue
makeVarArgs = makeFunc . Just . show
