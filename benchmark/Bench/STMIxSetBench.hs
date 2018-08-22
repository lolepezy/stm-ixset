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

import qualified Bench.TestData as TD
import qualified Data.IxSet.Typed as IXT

rows :: Int = 100000
chunkSize :: Int = 10000

ixSetBench :: IO ()
ixSetBench = do
  keys <- MWC.runWithCreate $ replicateM rows intKeyGenerator
  defaultMain [
      bgroup "STM IxSet" [
        bench "concurrent insert" $ nfIO $ stmIxSetinsert keys,
        bench "concurrent insert+delete" $ nfIO $ stmIxSetInsertDelete keys,
        bench "concurrent insert+select" $ nfIO $ stmIxSetInsertSelect keys
      ],
      bgroup "TVar IxSet" [
       bench "concurrent insert" $ nfIO $ ixSetInsert keys,
       bench "concurrent insert+delete" $ nfIO $ ixSetInsertDelete keys,
       bench "concurrent insert+select" $ nfIO $ ixSetInsertSelect keys
      ]
    ]


stmIxSetinsert keys = do
  m <- atomically TD.mkIdxSet
  let chunks = S.chunksOf chunkSize $ map TD.mkEntry keys
  as <- forM chunks $ \c -> async $ forM c $ \e -> atomically $ Ixs.insert m e
  forM_ as wait

stmIxSetInsertDelete keys = do
  m <- atomically TD.mkIdxSet
  let chunks = S.chunksOf chunkSize $ map TD.mkEntry keys
  as <- forM chunks $ \c -> async $ forM c $ \e -> atomically $ Ixs.insert m e
  forM_ as wait
  as <- forM chunks $ \c -> async $ forM c $ \e -> atomically $ Ixs.remove m e
  forM_ as wait

stmIxSetInsertSelect keys = do
  m <- atomically TD.mkIdxSet
  let chunks = S.chunksOf chunkSize $ map TD.mkEntry keys
  as <- forM chunks $ \c -> async $ forM c $ \e -> atomically $ Ixs.insert m e
  forM_ as wait
  replicateM_ 20 $ do
    as <- forM chunks $ \c -> async $ forM c $ \e -> atomically $ Ixs.get m (TD.field1 e)
    forM_ as wait

ixSetInsert keys = do
  m <- atomically $ newTVar (IXT.empty :: TD.IxEntry)
  let chunks = S.chunksOf chunkSize $ map TD.mkEntry keys
  as <- forM chunks $ \c -> async $ forM c $ \e ->
                           atomically $ modifyTVar' m $ IXT.insert e
  forM_ as wait

ixSetInsertDelete keys = do
  m <- atomically $ newTVar (IXT.empty :: TD.IxEntry)
  let chunks = S.chunksOf chunkSize $ map TD.mkEntry keys
  as <- forM chunks $ \c -> async $ forM c $ \e ->
                            atomically $ modifyTVar' m $ IXT.insert e
  forM_ as wait
  as <- forM chunks $ \c -> async $ forM c $ \e ->
                            atomically $ modifyTVar' m $ IXT.delete e
  forM_ as wait

ixSetInsertSelect keys = do
  m <- atomically $ newTVar (IXT.empty :: TD.IxEntry)
  let chunks = S.chunksOf chunkSize $ map TD.mkEntry keys
  as <- forM chunks $ \c -> async $ forM c $ \e ->
                            atomically $ modifyTVar' m $ IXT.insert e
  forM_ as wait
  replicateM_ 20 $ do
    as <- forM chunks $ \c -> async $ forM c $ \e ->
                              atomically $ do
                                x <- readTVar m
                                return $ IXT.getEQ (TD.field1 e) x
    forM_ as wait



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
