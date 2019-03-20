module Block2 where

import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, local)
import Control.Monad.Catch (throwM, Exception)
import Text.Read (readMaybe)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

data Command = Push Int | Pop | Add | Exit
  deriving (Read, Show)

data StackError = UnexpectedStack
  deriving Show

instance Exception StackError

main :: IO ()
main = runReaderT stackApp [] >>= putStrLn . ("Final stack " <>) . show

stackApp :: ReaderT [Int] IO [Int]
stackApp = do
  cmd <- liftIO getLine
  maybe (liftIO (putStrLn "Wrong command")
          >> stackApp)
        handleCommand $ readMaybe cmd

handleCommand :: Command -> ReaderT [Int] IO [Int]
handleCommand (Push x) = local (x:) stackApp
handleCommand Pop = local tail stackApp
handleCommand Add =
  local
    (\x ->
       let a = head x
        in let b = x !! 1
            in (a + b) : tail (tail x))
    stackApp
handleCommand Exit = ask