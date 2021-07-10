{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Evaluator where

import Control.Monad
import Control.Monad.Except
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import Evaluator.Environment
import Evaluator.Primitive.Boolean
import Evaluator.Primitive.IO
import Evaluator.Primitive.List
import Evaluator.Primitive.Number
import Evaluator.Primitive.String
import Evaluator.Primitive.Symbol
import Evaluator.Primitive.Type
import GHC.IO.IOMode
import Safe
import Types

eval :: IORef Environment -> LispValue -> IOResult LispValue
eval _ val@(StringValue _) = return val
eval _ val@(NumberValue _) = return val
eval _ val@(BooleanValue _) = return val
eval _ val@(CharacterValue _) = return val
eval envPtr (SymbolValue name) = getVar envPtr name
eval _ (ListValue [SymbolValue "quote", val]) = return val
eval envPtr (ListValue (SymbolValue "cond" : vals)) = cond envPtr vals
eval envPtr (ListValue (SymbolValue "case" : val : clauses)) = case' envPtr val clauses
eval envPtr (ListValue [SymbolValue "if", pred, then', else']) = do
  res <- eval envPtr pred
  case res of
    BooleanValue False -> eval envPtr else'
    _ -> eval envPtr then'
eval envPtr (ListValue [SymbolValue "set!", SymbolValue name, value]) = do
  val <- eval envPtr value
  setVar envPtr name val
eval envPtr (ListValue [SymbolValue "define", SymbolValue name, value]) = do
  val <- eval envPtr value
  defineVar envPtr name val
eval envPtr (ListValue (SymbolValue "define" : ListValue (SymbolValue name : params) : body)) = do
  func <- makeNormalFunc envPtr params body
  defineVar envPtr name func
eval envPtr (ListValue (SymbolValue "lambda" : ListValue params : body)) =
  makeNormalFunc envPtr params body
eval envPtr (ListValue (SymbolValue "lambda" : varargs@(SymbolValue _) : body)) =
  makeVarArgs varargs envPtr ([] :: [LispValue]) body
eval envPtr (ListValue [SymbolValue "load", StringValue filename]) = do
  contents <- load filename
  last <$> traverse (eval envPtr) contents
eval
  envPtr
  ( ListValue
      ( SymbolValue "define"
          : DottedListValue
              (SymbolValue name : params)
              varargs
          : body
        )
    ) = do
    func <- makeVarArgs varargs envPtr params body
    defineVar envPtr name func
eval envPtr (ListValue (func : args)) = do
  args <- traverse (eval envPtr) args
  func <- eval envPtr func
  apply func args
eval _ bsf = throwError $ BadSpecialFormError "Not yet implemented" bsf

apply :: LispValue -> [LispValue] -> IOResult LispValue
apply (PrimitiveFunctionValue func) args = liftResult $ func args
apply FunctionValue {..} args =
  if length fParams /= length args && isNothing fVararg
    then throwError $ NumArgsError (genericLength fParams) args
    else do
      x <- liftIO $ bindVars fClosure $ zip fParams args
      y <- bindVarArgs fVararg x
      evalBody y
  where
    evalBody envPtr = last <$> traverse (eval envPtr) fBody
    remainingArgs = drop (length fParams) args
    bindVarArgs arg envPtr = case arg of
      Just argName -> liftIO $ bindVars envPtr [(argName, ListValue remainingArgs)]
      Nothing -> return envPtr
apply (IOFunctionValue func) args = func args
apply notAFunction _ = throwError $ TypeMismatchError "function" notAFunction

--apply FunctionValue {..} args = case builtins fName of
--Just f -> liftResult $ f args
--Nothing -> throwError $ NotFunctionError fName "Builtin does not exist"

data CondClause
  = ArrowClause LispValue LispValue
  | StandardClause LispValue [LispValue]
  | ElseClause [LispValue]

cond :: IORef Environment -> SchemeIOFunction
cond envPtr clauses = do
  let parser clause = case clause of
        (SymbolValue "else" : exprs) -> Right $ ElseClause exprs
        (test : SymbolValue "=>" : [expr]) -> Right $ ArrowClause test expr
        (test : exprs) -> Right $ StandardClause test exprs
        _ -> Left $ BadSpecialFormError "`cond` clause must be of the form `(test => expr)` or `(test expr*)`" (ListValue clause)
  clauses <- liftResult $ traverse valueToList clauses
  clauses <- liftResult $ traverse parser clauses

  let doClause clause = case clause of
        ArrowClause test expr -> do
          test' <- eval envPtr test
          res <- liftResult $ valueToBool test'
          return
            if res
              then
                let evaledExprs =
                      do
                        v <- eval envPtr expr
                        return [eval envPtr $ ListValue [v, test']]
                        `catchError` \e -> return [throwError e]
                 in Just evaledExprs
              else Nothing
        StandardClause test [] -> do
          test' <- eval envPtr test
          res <- liftResult $ valueToBool test'
          return
            if res
              then Just $ return [eval envPtr test']
              else Nothing
        StandardClause test exprs -> do
          test' <- eval envPtr test
          res <- liftResult $ valueToBool test'
          return
            if res
              then
                let a = eval envPtr <$> exprs
                 in Just $ return a
              else Nothing
        ElseClause exprs -> return $ Just $ return $ eval envPtr <$> exprs
  parsed <- traverse doClause clauses
  let toExecute = headMay $ catMaybes parsed
  case toExecute of
    Just xs -> do
      -- execute them all, return the last
      xs <- sequenceA =<< xs
      evaled <- traverse (eval envPtr) xs
      return $ last evaled
    Nothing -> return $ SymbolValue "nil" -- return value unspecified (nil)

data CaseClause
  = StandardCaseClause LispValue [LispValue]
  | ElseCaseClause [LispValue]

extractDatum :: CaseClause -> Maybe LispValue
extractDatum clause = case clause of
  StandardCaseClause d _ -> Just d
  _ -> Nothing

case' :: IORef Environment -> LispValue -> [LispValue] -> IOResult LispValue
case' envPtr val clauses' = do
  let parser clause = case clause of
        (SymbolValue "else" : exprs) -> Right $ ElseCaseClause exprs
        (datum : exprs) -> Right $ StandardCaseClause datum exprs
        _ -> Left $ BadSpecialFormError "`case` clause must be of the form `(datum expr*)` or `(else expr*)`" $ ListValue clause
  clauses <- liftResult $ traverse valueToList clauses' >>= traverse parser

  let datums = catMaybes $ extractDatum <$> clauses
      allUnique = nub datums == datums

  unless allUnique $ throwError $ BadSpecialFormError "all datums in a case expression must be unique" $ ListValue clauses'

  let doClause clause = case clause of
        StandardCaseClause datum exprs -> do
          datum' <- eval envPtr datum
          return
            if datum' == val
              then Just $ eval envPtr <$> exprs
              else Nothing
        ElseCaseClause exprs -> do
          return $ Just $ eval envPtr <$> exprs
  parsed <- traverse doClause clauses
  let toExecute = headMay $ catMaybes parsed
  case toExecute of
    Just xs -> do
      xs <- sequenceA xs
      evaled <- traverse (eval envPtr) xs
      return $ last evaled
    Nothing -> return $ SymbolValue "nil"

applyProc :: SchemeIOFunction
applyProc [func, ListValue args] = apply func args
applyProc (func : args) = apply func args
applyProc bal = throwError $ NotEnoughArgsError 1 bal

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
  [ ("apply", applyProc),
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
