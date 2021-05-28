module Main where

import           MyFirstModule

main :: IO ()
main = do
  input <- getLine
  let repeated = replicate 3 input
  print repeated
