{-# OPTIONS -Wall -fwarn-tabs #-}
module GlobToRegex (
  globToRegex
) where

import Control.Monad

import Logger

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
