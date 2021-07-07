module Util where

unreachable :: a
unreachable = error "Entered unreachable code unreachable"

wrapError :: Maybe b -> a -> Either a b
wrapError Nothing err = Left err
wrapError (Just val) _ = Right val
