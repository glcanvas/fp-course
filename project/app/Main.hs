module Main (
    main
) where

import Game(executeGame)
import System.Environment(getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then executeGame Nothing
    else executeGame (Just $ head args)