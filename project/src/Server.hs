{-# LANGUAGE Strict #-}

module Server where

import Network.Simple.TCP


serverCreate :: IO ()
serverCreate = serve (Host "127.0.0.1") "8080" aaa

aaa :: (Socket, SockAddr) -> IO ()
aaa (x, y) = do
  print x
  print y



connectToServer :: IO ()
connectToServer = connect ("127.0.0.1") "8080" aaa