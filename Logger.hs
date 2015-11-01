{-# OPTIONS -Wall -fwarn-tabs #-}
module Logger (
  Logger
  , Log
  , runLogger
  , record
) where

import Control.Monad
import Text.Regex.Posix ((=~))

globToRegex :: String -> Logger String
globToRegex cs = do
  ds <- globToRegex' cs
  return ('^':ds)

globToRegex' :: String -> Logger String
globToRegex' "" = return ""
globToRegex' ('?':cs) = do
  record "any"
  ds <- globToRegex' cs
  return ('.':ds)
globToRegex' ('*':cs) = do
  record "kleene star"
  ds <- globToRegex' cs
  return (".*" ++ ds)
globToRegex' ('[':'!':c:cs) = do
  record "character class, negative"
  ds <- charClass cs
  return ("[^" ++ c : ds)
globToRegex' ('[':c:cs) = do
  record "character class"
  ds <- charClass cs
  return ("[" ++ c:ds)
globToRegex' ('[':_) =
  fail "unterminated character class"
globToRegex' (c:cs) = liftM2 (++) (escape c) (globToRegex' cs)
charClass :: String -> Logger String
charClass (']':cs) = (']':)`liftM` globToRegex' cs
charClass (c:cs) = (c:) `liftM` charClass cs
charClass [] = error "unterminated character class"

escape :: Char -> Logger String
escape c | c `elem` regexChars = record "escape" >> return ('\\' : [c])
         | otherwise = return [c]
  where regexChars = "\\+()^$.{}]|"

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

