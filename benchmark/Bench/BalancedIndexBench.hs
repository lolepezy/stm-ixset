{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Bench.BalancedIndexBench where

import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async

import Data.Maybe (catMaybes)
import Data.List ((\\), nub, sort)

import Criterion.Main
import Criterion.Monad
import Criterion.IO

import qualified Data.Map.Strict as M
import qualified System.Random.MWC.Monad as MWC
import qualified Data.Text as Text
import qualified Data.Char as Char

import qualified Data.List.Split as S

import qualified Control.Concurrent.STM.Stats as Stat

import Data.IxSet.BalancedIndex as Ix

import qualified Bench.TestData as TD

rows :: Int = 100000
chunkSize :: Int = 10000

indexBench :: IO ()
indexBench = do
  keys <- MWC.runWithCreate $ replicateM rows textKeyGenerator
  let keySetSizes = [1..9] ++ [11, 12, 13, 14, 15, 100, 1000, 10000, 100 * 1000]
  aLotOfKeys <- MWC.runWithCreate $ replicateM (maximum keySetSizes) intKeyGenerator
  forM_ keySetSizes $ \len -> do
    let keys' = take len aLotOfKeys
    bigStm <- bigSet keys' Ix.new Ix.insert
    d      <- atomically $ Ix.depth bigStm
    stat   <- atomically $ Ix.stats bigStm
    dump   <- atomically $ Ix.dumpLine bigStm
    print $ "key number = " ++ show len ++ " depth = " ++ show d ++ 
            ", stats = " ++ show stat -- ++ " dump = " ++ show dump

  defaultMain [
    bgroup "Balanced STM Index Map" [
        bench "insert" $ nfIO $ do
           m <- atomically Ix.new
           let chunks = S.chunksOf chunkSize keys
           as <- forM chunks $ \c -> async $ forM c $ \k -> atomically $ Ix.insert m k ()
           forM_ as wait
        {-,
         bench "insert+delete" $ nfIO $ do
            m <- atomically Ix.new
            let chunks = S.chunksOf chunkSize keys
            as <- forM chunks $ \c -> async $ forM c $ \k -> atomically $ Ix.insert m k ()
            forM_ as wait
            as <- forM chunks $ \c -> async $ forM c $ \k -> atomically $ Ix.remove m k
            forM_ as wait -}
       ], 
       bgroup "TVar Map" [
           bench "insert" $ nfIO $ do
              m <- atomically $ newTVar M.empty
              let chunks = S.chunksOf chunkSize keys
              as <- forM chunks $ \c -> async $ forM c $ \k ->
                                        atomically $ modifyTVar' m $ M.insert k ()
              forM_ as wait
            ,
            bench "insert+delete" $ nfIO $ do
               m <- atomically $ newTVar M.empty
               let chunks = S.chunksOf chunkSize keys
               as <- forM chunks $ \c -> async $ forM c $ \k ->
                                         atomically $ modifyTVar' m $ M.insert k ()
               forM_ as wait
               as <- forM chunks $ \c -> async $ forM c $ \k ->
                                         atomically $ modifyTVar' m $ M.delete k
               forM_ as wait
        ]
    ]

bigSet keys create insert = do
  m <- atomically create
  forM_ keys $ \k -> atomically (insert m k ("val" ++ show k))
  return m

textKeyGenerator :: MWC.Rand IO Text.Text
textKeyGenerator = do
  l <- length
  s <- replicateM l char
  return $! Text.pack s
  where
    length = MWC.uniformR (7, 20)
    char = Char.chr <$> MWC.uniformR (Char.ord 'a', Char.ord 'z')

intKeyGenerator :: MWC.Rand IO Int
intKeyGenerator = MWC.uniformR (1, 1000000000) >>= \x -> return $! x
  -- l <- length
  -- s <- replicateM l char
  -- return $! Text.pack s
  -- where
  --   length = MWC.uniformR (7, 20)
  --   char = Char.chr <$> MWC.uniformR (Char.ord 'a', Char.ord 'z')
