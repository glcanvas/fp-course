module Main where

import System.Environment
import Block1

main :: IO ()
main = do
  args <- getArgs
  executeScript args


