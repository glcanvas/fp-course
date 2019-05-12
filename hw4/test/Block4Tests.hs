{-# LANGUAGE NumericUnderscores #-}
module Block4Tests (
    testsCHT
  , bigTestsCHT
) where

import Test.Hspec
import Block4

testsCHT :: IO ()
testsCHT = do
  cht <- newCHT :: (IO (ConcurrentHashTable Integer String))
  hspec $
    describe "simple" $ do
      it "zero" $ do
        zero <- sizeCHT cht
        zero `shouldBe` (0 :: Int)
      it "single element" $ do
        _ <- putCHT 1 "0" cht
        one <- sizeCHT cht
        one `shouldBe` (1 :: Int)
      it "add a lot of elements" $ do
        mapM_ (\i -> putCHT i (show i) cht) [0..100]
        sto <- sizeCHT cht
        sto `shouldBe` (101 :: Int)
      it "rewrite elements" $ do
        _ <- putCHT 1 "1" cht
        _ <- putCHT 1 "2" cht
        v <- getCHT 1 cht
        v `shouldBe` (Just "2" :: Maybe String)


bigTestsCHT :: IO ()
bigTestsCHT = do
  cht <- newCHT :: (IO (ConcurrentHashTable Integer String))
  hspec $
    describe "80% read 20% write in 10^5 requests" $ do
      it "add" $
        mapM_
          (\i -> do
             putCHT i (show i) cht
             sz <- sizeCHT cht
             toInteger sz `shouldBe` (i + 1))
          [0 .. 20_000]
      it "read" $
        mapM_
          (\i -> do
            value <- getCHT i cht
            if i <= 20_000
              then value `shouldBe` Just (show i)
              else value `shouldBe` Nothing)
          [0 .. 80_000]