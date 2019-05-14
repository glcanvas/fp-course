module Block4Bench (
    readCHT
  , writeCHT
  , concurrentReadCHT
  , concurrentReadWriteCHT
  , concurrentWriteCHT
) where

import Block4

import Criterion.Main
import Control.Concurrent.Async

readCHT :: IO ()
readCHT = defaultMain [bgroup "read tests"
  $ map (\x -> bench ("read " <> show x) $ nfIO (inner x)) [100, 1000, 10000, 100000]]
  where
    inner :: Integer -> IO ()
    inner x = do
      cht <- newCHT :: (IO (ConcurrentHashTable Integer String))
      mapM_ (`getCHT` cht) [0 .. x]


writeCHT :: IO ()
writeCHT = defaultMain [bgroup "write tests"
  $ map (\x -> bench ("write " <> show x) $ nfIO (inner x)) [100, 1000, 2000, 4000, 5000, 10000, 20000]]
  where
    inner :: Integer -> IO ()
    inner x = do
      cht <- newCHT :: (IO (ConcurrentHashTable Integer String))
      mapM_ (\y -> putCHT y (show y) cht) [0 .. x]


concurrentReadCHT :: IO ()
concurrentReadCHT = defaultMain [bgroup "concurrent read tests"
  $ map (\x -> bench ("read concurrent " <> show x) $ nfIO (inner x)) [100, 1000, 10000, 100000]]
  where
    inner :: Integer -> IO ()
    inner x = do
      cht <- newCHT :: (IO (ConcurrentHashTable Integer String))
      trds <- mapM (\y -> async $ do
        v <- getCHT y cht
        case v of
          Nothing -> pure ()
          Just _ -> error "ogo") [0 .. x]
      mapM_ wait trds

concurrentWriteCHT :: IO ()
concurrentWriteCHT = defaultMain [bgroup "concurrent write "
  $ map (\x -> bench ("write concurrent " <> show x) $ nfIO (inner x)) [100, 1000, 2000]]
  where
    inner :: Integer -> IO ()
    inner x = do
      cht <- newCHT :: (IO (ConcurrentHashTable Integer String))
      trds <- mapM (\y -> async $ do
        putCHT y (show y) cht
        size' <- sizeCHT cht
        putStrLn $ show y <> " end"
        if size' <= 0
          then error "ogo"
          else pure ()) [0 .. x]
      mapM_ wait trds

concurrentReadWriteCHT :: IO ()
concurrentReadWriteCHT = defaultMain [bgroup "concurrent 20 % write 80% read"
  $ map (\x -> bench ("combine concurrent " <> show x) $ nfIO (inner x 32 newCHT)) [10000, 100000]]
  where
    inner :: Integer -> Integer -> IO (ConcurrentHashTable Integer String) -> IO ()
    inner ops threads cht =
      let singleWrite =  round (0.2 * toRational ops) `div` threads in
      let singleRead = round (0.8 * toRational ops) `div` threads in
      mapM (\_ -> async $ singleThread singleWrite singleRead) [0 .. threads] >>= mapM_ wait
      where
        singleThread writeOps readOps =
          let cycle' = (writeOps `mod` readOps) + 1 in do
          table <- cht
          mapM (\y ->
            if y `mod` cycle'  == 0
              then putCHT y (show y) table
              else
                do
                  v <- getCHT y table
                  case v of
                    Nothing -> pure ()
                    Just _ -> error "ogo") [0 .. writeOps + readOps]
