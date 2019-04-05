module Prac where

import Control.Concurrent.MVar
import Control.Concurrent
import Control.Exception(throwIO)
import Control.Monad
import Control.Applicative
import Control.Exception


blia f = forkIO f >> putStrLn "ok"

blia' = blia (putStrLn "2")