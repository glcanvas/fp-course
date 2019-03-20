module Main where

import Block1
import Block2

main :: IO ()
main = do
  let x = stringSum "1"
  case x of
    Nothing -> putStrLn "err"
    Just r -> print r

