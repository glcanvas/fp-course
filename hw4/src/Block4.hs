{-# LANGUAGE Strict #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Block4 (
    ConcurrentHashTable(..)
  , newCHT
  , getCHT
  , putCHT
  , sizeCHT
) where

import Control.Monad
import Control.Concurrent.STM
import Data.Array
import Control.Exception
import Data.Hashable

-- | data type for ConcurrentHashTable
data ConcurrentHashTable k v = ConcurrentHashTable {
  size :: TVar Int,
  capacity :: TVar Int,
  elements :: TVar (Array Int (TVar [(k,v)]))
}

-- | function that create new ConcurrentHashTable
newCHT :: Hashable k => Eq k => Eq v => IO (ConcurrentHashTable k v)
newCHT =
  mask_ $ atomically $ do
    let capacityLength = 2048
    size' <- newTVar (0 :: Int)
    capacity' <- newTVar (capacityLength :: Int)
    slots' <- replicateM (capacityLength + 1) (newTVar [])
    let arrayHash = listArray (0, capacityLength) slots'
    elements' <- newTVar arrayHash
    pure $ ConcurrentHashTable {size = size', capacity = capacity', elements = elements'}

-- | get element from ConcurrentHashTable
getCHT :: Hashable k => Eq k => Eq v => k -> ConcurrentHashTable k v -> IO (Maybe v)
getCHT key (ConcurrentHashTable _ htCapacity htElements) =
  mask_ $ atomically $ do
    capacity' <- readTVar htCapacity
    let getPosition = hash key `mod` capacity'
    array' <- readTVar htElements
    slots' <- readTVar (array' ! getPosition)
    pure $ lookup key slots'

-- | add or rewrite element in ConcurrentHashTable
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

-- | get size of ConcurrentHashTable
sizeCHT :: Hashable k => Eq k => Eq v => ConcurrentHashTable k v -> IO Int
sizeCHT (ConcurrentHashTable htSize _ _) = mask_ $ atomically $ readTVar htSize

replaceSlotByKey :: Hashable k => Eq k => Eq v => k -> v -> [(k,v)] -> [(k,v)]
replaceSlotByKey key value = inner
  where
    inner (link@(key', _) : xs) =
      if key == key'
        then (key, value) : xs
        else link : inner xs
    inner [] = []

needResize :: Int -> Int -> Bool
needResize keys capacitys = fromIntegral capacitys * (0.9 ::Double) <= fromIntegral keys

resizeTable' :: Hashable k => Eq k => Eq v => Int -> Array Int (TVar [(k,v)]) -> STM(Array Int (TVar [(k,v)]))
resizeTable' oldCap' oldArr' =
  let newCap = oldCap' * 2 in
  replicateM (newCap + 1) (newTVar [])
    >>=(\buckets ->
      let arrayHash = listArray (0, newCap) buckets in
        resizeArray' oldArr' arrayHash oldCap' newCap
          >> pure arrayHash)
  where
    resizeArray' _ _ (-1) _ = pure ()
    resizeArray' oldArr newArr oldCap newCap = do
      curList <- readTVar $ oldArr ! oldCap
      if not $ null curList
        then pasteList curList newArr newCap >> resizeArray' oldArr newArr (oldCap - 1) newCap
        else resizeArray' oldArr newArr (oldCap - 1) newCap
    pasteList list newArr newCap = inner list
      where
        inner [] = pure ()
        inner ((key, value) : _) =
          let hashCode = hash key `mod` newCap in
            readTVar (newArr ! hashCode)
              >>= (\existElements ->
                    let newList = (key, value) : existElements in
                      writeTVar (newArr ! hashCode) newList)