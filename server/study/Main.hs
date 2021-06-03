module Main where

import           MyFirstModule


putQStrLn = do
  str <- getLine
  putChar '"'
  putStr str
  putChar '"'
  putChar '\n'

main = putQStrLn
