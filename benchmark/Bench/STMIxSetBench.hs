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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Bench.STMIxSetBench where

import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async

import Data.Maybe (catMaybes)
import Data.List ((\\), nub, sort)
import Data.Hashable

import GHC.Generics (Generic)

import Criterion.Main
import Criterion.Monad
import Criterion.IO

import qualified Data.Map.Strict as M
import qualified System.Random.MWC.Monad as MWC
import qualified Data.Text as Text
import qualified Data.Char as Char

import qualified Data.List.Split as S

import qualified Control.Concurrent.STM.Stats as Stat

import Data.IxSet.Index as Ix
import Data.IxSet.STM as Ixs

import Bench.TestData as TD

rows :: Int = 100000
chunkSize :: Int = 10000

ixSetBench :: IO ()
ixSetBench = do
  keys <- MWC.runWithCreate $ replicateM rows intKeyGenerator
  defaultMain [
    bgroup "STM IxSet" [
        bench "insert" $ nfIO $ do
           m <- atomically TD.mkIdxSet
           let chunks = S.chunksOf chunkSize keys
           as <- forM chunks $ \c -> async $ forM c $ \k -> atomically $ Ixs.insert m (mkEntry k)
           forM_ as wait
        ,
        bench "insert+delete" $ nfIO $ do
            m <- atomically TD.mkIdxSet
            let chunks = S.chunksOf chunkSize keys
            as <- forM chunks $ \c -> async $ forM c $ \k -> atomically $ Ixs.insert m (mkEntry k)
            forM_ as wait
            as <- forM chunks $ \c -> async $ forM c $ \k -> atomically $ Ixs.remove m (mkEntry k)
            forM_ as wait
       ],
       bgroup "TVar IxSet" [
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
