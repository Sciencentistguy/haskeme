module Util where

import Control.Monad.Except

unreachable :: a
unreachable = error "Entered unreachable code unreachable"

wrapError :: Maybe b -> a -> Either a b
wrapError Nothing err = Left err
wrapError (Just val) _ = Right val

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = action `catchError` (return . show)
