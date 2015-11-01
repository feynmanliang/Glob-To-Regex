{-# OPTIONS -Wall -fwarn-tabs #-}
module Logger (
  Logger
  , Log
  , runLogger
  , record
) where

import Control.Monad

-- `newtype` ~== `data`, except newtype is restricted to single constructor with
-- one field
-- this makes the newtype isomorphic to the type of the field, removing data
-- constructor overhead
newtype Logger a = Logger { execLogger :: (a, Log) }

-- `type` creates a type alias, `newtype` declares a new type
type Log = [String]

instance Functor Logger where
  fmap = liftM

instance Applicative Logger where
  pure = return
  (<*>) = ap

instance Monad Logger where
  return a = Logger (a,[])
  (Logger (a, l)) >>= f = case f a of
    Logger (b,l') -> Logger (b,l ++ l')

runLogger :: Logger a -> (a, Log)
runLogger = execLogger

record :: String -> Logger ()
record s = Logger ((), [s])
