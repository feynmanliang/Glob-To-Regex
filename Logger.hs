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

data Logger a = L (a, Log)
type Log = [String]

instance Functor Logger where
  fmap = liftM

instance Applicative Logger where
  pure = return
  (<*>) = ap

instance Monad Logger where
  return a = L (a,[])
  (L (a, l)) >>= f = case f a of
    L (b,l') -> L (b,l' ++ l)

runLogger :: Logger a -> (a, Log)
runLogger (L (a,l)) = (a,reverse l)

record :: String -> Logger ()
record s = L ((), [s])

