module Main where

import Types
import Solution

main :: IO ()
main = do
  s <- getLine
  print $ typeOf (read s :: Term)
