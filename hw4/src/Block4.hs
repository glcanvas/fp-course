{-# LANGUAGE Strict #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification#-}
module Block4 where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TArray
import Data.Array
import Control.Monad.ST
import Control.Loop
import Data.STRef
import GHC.Arr(foldlElems')

data ConcurrentHashTable k v = ConcurrentHashTable {
  size :: TVar Int,
  capacity :: TVar Int,
  elements :: TVar (Array Int (TVar [(k,v)]))
}


newCHT :: Eq k => Eq v => IO (ConcurrentHashTable k v)
newCHT =
  atomically $ do
    size' <- newTVar (0 :: Int)
    capacity' <- newTVar (10 :: Int)
    cap <- readTVar capacity'
    slots' <- replicateM 10 (newTVar [])
    let arrayHash = listArray (0, 10) slots'
    elements' <- newTVar arrayHash
    pure $ ConcurrentHashTable {size = size', capacity = capacity', elements = elements'}


getCHT :: Enum k => Eq k => Eq v => k -> ConcurrentHashTable k v -> IO (Maybe v)
getCHT key table@(ConcurrentHashTable htSize htCapacity htElements) =
  atomically $ do
    capacity' <- readTVar htCapacity
    let getPosition = fromEnum key `mod` capacity'
    array' <- readTVar htElements
    slots' <- readTVar (array' ! getPosition)
    pure $ lookup key slots'

putCHT ::Enum k => Eq k => Eq v => k -> v -> ConcurrentHashTable k v -> IO ()
putCHT key value (ConcurrentHashTable htSize htCapacity htElements) =
  atomically $ do
    capacity' <- readTVar htCapacity
    let getPosition = fromEnum key `mod` capacity'
    size' <- readTVar htSize
    array' <- readTVar htElements
    slots' <- readTVar (array' ! getPosition)
    let item = lookup key slots'
    case item of
      Just itemValue ->
        if itemValue == value
          then pure ()
          else writeTVar (array' ! getPosition) (replaceSlotByKey key value slots')
      Nothing ->
        let updSlots = (key, value) : slots' in
        writeTVar (array' ! getPosition) updSlots
        >> writeTVar htSize (size' + 1)
        >> if needResize (size' + 1) capacity'
            then do
              (cap, updArr) <- resizeTable capacity' array'
              writeTVar htCapacity cap
              writeTVar htElements updArr
              pure ()
            else pure ()

sizeCHT :: ConcurrentHashTable k v -> IO Int
sizeCHT (ConcurrentHashTable htSize _ _) = atomically $ readTVar htSize

replaceSlotByKey :: Enum k => Eq k => Eq v => k -> v -> [(k,v)] -> [(k,v)]
replaceSlotByKey key value = inner
  where
    inner (link@(key', value') : xs) =
      if key == key'
        then (key, value) : xs
        else link : inner xs
    inner [] = []

needResize :: Int -> Int -> Bool
needResize keys capacitys = fromIntegral capacitys * 0.75 <= fromIntegral keys

resizeTable :: Enum k => Eq k => Eq v => Int ->  Array Int (TVar [(k,v)]) -> STM (Int, Array Int (TVar [(k,v)]))
resizeTable htCapacity htElements = do
  let newCap' = htCapacity * 2
  allElements <- foldlElems' (\b a -> do
      tmp' <- readTVar a
      tmp <- b
      pure $ tmp' <> tmp) (pure []) htElements
  slots' <- compose newCap' allElements
  let arrayHash = listArray (0, newCap') slots'
  pure (newCap', arrayHash)


compose :: Enum k => Eq k => Eq v => Int -> [(k,v)] -> STM [TVar [(k,v)]]
compose capac a = runST $ do
  arr <- newSTRef a
  emptyArray <- newSTRef [] -- ([]::[TVar [(k, v)]])
  numLoop 0 (capac - 1) (\i -> do
    xxx <- readSTRef arr
    let (filtered, other) = customFilterByKey i xxx
    builded <- readSTRef emptyArray
    writeSTRef emptyArray (newTVar filtered: builded)
    writeSTRef arr other)
  v <- readSTRef emptyArray
  return $ sequenceA v
  where
    customFilterByKey :: Enum k => Eq k => Eq v => Int -> [(k,v)] -> ([(k,v)], [(k,v)])
    customFilterByKey key array =
      let func (x, _) = fromEnum x == key in
      let left = filter func array in
      let right = filter (not . func) array in
          (left, right)


aaaa :: IO ()
aaaa = do
  cht <- newCHT :: (IO (ConcurrentHashTable Integer String))
  putCHT 0 "a" cht
  putCHT 1 "b" cht
  putCHT 2 "c" cht
  putCHT 3 "d" cht
  res <- getCHT 0 cht
  res' <- getCHT 1 cht
  res'' <- getCHT 2 cht
  res''' <- getCHT 1488 cht
  print res
  print res'
  print res''
  print res'''
