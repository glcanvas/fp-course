{-# LANGUAGE Strict #-}

module Server where

import Network.Simple.TCP
import Utils (readProperties, checkKeys)

import Data.Text.Encoding
import Data.IORef
import Control.Concurrent.STM
import Control.Concurrent
import Data.Maybe

import qualified Data.Map as Map
import qualified Data.ByteString as Bytes
import qualified Data.Text as Text

import Data.Maybe


data UsersEnvironment = UsersEnvironment {
  onlineUsers :: TVar (Map.Map Bytes.ByteString SockAddr)
  , threadMapping :: TVar (Map.Map Bytes.ByteString ThreadId)

}

newUsersEnvironment :: IO UsersEnvironment
newUsersEnvironment = atomically $ do
  users <- newTVar Map.empty
  threads <- newTVar Map.empty
  pure (UsersEnvironment users threads)

serverCreate :: String -> String -> IO ()
serverCreate host port = serve (Host host) port (`registerUser` newUsersEnvironment)

registerUser :: (Socket, SockAddr) -> IO UsersEnvironment -> IO ()
registerUser (socket, socketAddress) userEnvironment = do
  userEnvironment' <- userEnvironment
  userName <- recv socket 10
  currentThreadId <- myThreadId
  case userName of
    Nothing -> closeSocketWithPrint x
    Just byteString ->
      atomically $ do
        xxx <- readTVar $ onlineUsers userEnvironment'
        undefined


connectToServer :: IO ()
connectToServer = connect ("127.0.0.1") "8080" bbb

bbb :: (Socket, SockAddr) -> IO ()
bbb (x, y) = do
  print x
  print y
  send x $ (encodeUtf8 . Text.pack) "privet"


closeSocketWithPrint :: Socket -> IO ()
closeSocketWithPrint s = do
  print ("begin close: " <> show s)
  closeSock s
  print $ "end close: " <> show s

configureServer :: FilePath -> IO ()
configureServer configPath = do
  configs <- readProperties configPath
  case configs of
    Nothing -> error $ "can'r read config file: " <> configPath
    Just properties ->
      checkKeys ["host", "port"] properties >>
          serverCreate (properties Map.! "host") (properties Map.! "port")