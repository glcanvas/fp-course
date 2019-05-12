{-# LANGUAGE Strict #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification#-}

module Block4 (
    ConcurrentHashTable(..)
  , newCHT
  , getCHT
  , putCHT
  , sizeCHT
  , aaaa
  , bbbb
) where

import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.STM.TArray
import Data.Array
import Control.Monad.ST
import Control.Loop
import Data.STRef
import GHC.Arr
import Control.Exception
import Data.Hashable


import Data.IORef
import Control.Concurrent

data ConcurrentHashTable k v = ConcurrentHashTable {
  size :: TVar Int,
  capacity :: TVar Int,
  elements :: TVar (Array Int (TVar [(k,v)]))
}

newCHT :: Hashable k => Eq k => Eq v => IO (ConcurrentHashTable k v)
newCHT =
  mask_ $ atomically $ do
    let capacity = 2048 -- 40000
    size' <- newTVar (0 :: Int)
    capacity' <- newTVar (capacity :: Int)
    cap <- readTVar capacity'
    slots' <- replicateM (capacity + 1) (newTVar [])
    let arrayHash = listArray (0, capacity) slots'
    elements' <- newTVar arrayHash
    pure $ ConcurrentHashTable {size = size', capacity = capacity', elements = elements'}


getCHT :: Hashable k => Eq k => Eq v => k -> ConcurrentHashTable k v -> IO (Maybe v)
getCHT key table@(ConcurrentHashTable htSize htCapacity htElements) =
  mask_ $ atomically $ do
    capacity' <- readTVar htCapacity
    let getPosition = hash key `mod` capacity'
    array' <- readTVar htElements
    slots' <- readTVar (array' ! getPosition)
    pure $ lookup key slots'

putCHT :: Hashable k => Eq k => Eq v => k -> v -> ConcurrentHashTable k v -> IO ()
putCHT key value (ConcurrentHashTable htSize htCapacity htElements) =
  mask_ $ atomically $ do
    capacity' <- readTVar htCapacity
    let getPosition = hash key `mod` capacity'
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
              updArr <- resizeTable' capacity' array'
              writeTVar htCapacity (capacity' * 2)
              writeTVar htElements updArr
              pure ()
            else pure ()

sizeCHT :: Hashable k => Eq k => Eq v => ConcurrentHashTable k v -> IO Int
sizeCHT (ConcurrentHashTable htSize _ _) = mask_ $ atomically $ readTVar htSize

replaceSlotByKey :: Hashable k => Eq k => Eq v => k -> v -> [(k,v)] -> [(k,v)]
replaceSlotByKey key value = inner
  where
    inner (link@(key', value') : xs) =
      if key == key'
        then (key, value) : xs
        else link : inner xs
    inner [] = []

needResize :: Int -> Int -> Bool
needResize keys capacitys = fromIntegral capacitys * 0.9  <= fromIntegral keys



resizeTable' :: Hashable k => Eq k => Eq v => Int -> Array Int (TVar [(k,v)]) -> STM(Array Int (TVar [(k,v)]))
resizeTable' oldCap oldArr =
  let newCap = oldCap * 2 in
  replicateM (newCap + 1) (newTVar [])
    >>=(\buckets ->
      let arrayHash = listArray (0, newCap) buckets in
        resizeArray' oldArr arrayHash oldCap newCap
          >> pure arrayHash)
  where
    resizeArray' oldArr newArr (-1) newCap = pure ()
    resizeArray' oldArr newArr oldCap newCap = do
      curList <- readTVar $ oldArr ! oldCap
      if not $ null curList
        then pasteList curList newArr newCap >> resizeArray' oldArr newArr (oldCap - 1) newCap
        else resizeArray' oldArr newArr (oldCap - 1) newCap
    pasteList list newArr newCap = inner list
      where
        inner [] = pure ()
        inner ((key, value) : xs) =
          let hashCode = hash key `mod` newCap in
            readTVar (newArr ! hashCode)
              >>= (\existElements ->
                    let newList = (key, value) : existElements in
                      writeTVar (newArr ! hashCode) newList)

aaaa :: Integer -> IO ()
aaaa xx = do
  cht <- newCHT :: (IO (ConcurrentHashTable Integer String))
  mapM_ (\i -> putCHT i (show i) cht) [0..xx]
  sto <- sizeCHT cht
  print sto
  ref <- newIORef 0
  cht <- newCHT :: (IO (ConcurrentHashTable Integer String))
  mapM_ (\i -> do
    x <- getCHT i cht
    w <- readIORef ref
    case x of
      Nothing -> writeIORef ref (w + 1)
      Just _ -> pure ()) [0..10000]
  sto <- sizeCHT cht
  print sto


bbbb :: Int -> IO ()
bbbb  xxx = do
  print xxx
  cht <- newCHT :: (IO (ConcurrentHashTable Integer String))
  mapM_ (\i -> forkIO $ do
    putCHT i "0" cht) [0..20000]
  sz <- sizeCHT cht
  print sz