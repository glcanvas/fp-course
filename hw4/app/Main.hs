module Main where

import Block1
import SimpleMult
import Block2
import Block4
import Data.IORef

main :: IO ()
main = bbbb 228
  {-do
  ref <- newIORef 0
  cht <- newCHT :: (IO (ConcurrentHashTable Integer String))
  mapM_ (\i -> do
    x <- getCHT i cht
    w <- readIORef ref
    case x of
      Nothing -> writeIORef ref (w + 1)
      Just _ -> pure ()) [0..1000000]
  sto <- sizeCHT cht
  ww <- readIORef ref
  print sto
  print ww

  -}