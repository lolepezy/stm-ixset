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

import Data.IxSet.Index as Ix
import Data.IxSet.BalancedIndex as BIx
import Data.IxSet.STM as Ixs

import qualified Bench.TestData as TD
import qualified Data.IxSet.Typed as IXT

rows :: Int = 100000
chunkSize :: Int = 10000

ixSetBench :: IO ()
ixSetBench = do
  keys       <- MWC.runWithCreate $ replicateM rows intKeyGenerator
  aLotOfKeys <- MWC.runWithCreate $ replicateM (500*1000) intKeyGenerator
  let aLotOfEntries = map TD.mkEntry aLotOfKeys
  bigStm     <- bigSet aLotOfKeys TD.mkIdxSet Ixs.insert
  bigIxSet   <- bigSet aLotOfKeys
                  (newTVar (IXT.empty :: TD.IxEntry))
                  (\m e -> modifyTVar' m $ IXT.insert e)

  defaultMain [
      bgroup "STM IxSet big" [
        bench "select field 1" $ nfIO $ forM_ aLotOfEntries $ \e -> atomically $ Ixs.get bigStm (TD.field1 e),
        bench "select field 2" $ nfIO $ forM_ aLotOfEntries $ \e -> atomically $ Ixs.get bigStm (TD.field2 e),
        bench "select field 3" $ nfIO $ forM_ aLotOfEntries $ \e -> atomically $ Ixs.get bigStm (TD.field3 e)
      ],
      bgroup "TVar IxSet big" [
        bench "select field 1" $ nfIO $ forM_ aLotOfEntries $ \e -> atomically $ getFromIxSet bigIxSet e TD.field1,
        bench "select field 2" $ nfIO $ forM_ aLotOfEntries $ \e -> atomically $ getFromIxSet bigIxSet e TD.field2,
        bench "select field 3" $ nfIO $ forM_ aLotOfEntries $ \e -> atomically $ getFromIxSet bigIxSet e TD.field3
      ]
      -- bgroup "STM IxSet sequential" [
      --   bench "insert" $ nfIO $ seqStmIxSetInsert keys,
      --   bench "insert+delete" $ nfIO $ seqStmIxSetInsertDelete keys,
      --   bench "insert+select" $ nfIO $ seqStmIxSetInsertSelect keys
      -- ],
      -- bgroup "STM IxSet concurrent" [
      --   bench "concurrent insert" $ nfIO $ stmIxSetInsert keys,
      --   bench "concurrent insert+delete" $ nfIO $ stmIxSetInsertDelete keys,
      --   bench "concurrent insert+select" $ nfIO $ stmIxSetInsertSelect keys
      -- ],
      -- bgroup "TVar IxSet sequential" [
      --  bench "insert" $ nfIO $ seqIxSetInsert keys,
      --  bench "insert+delete" $ nfIO $ seqIxSetInsertDelete keys,
      --  bench " insert+select" $ nfIO $ seqIxSetInsertSelect keys
      -- ],
      -- bgroup "TVar IxSet concurrent" [
      --  bench "concurrent insert" $ nfIO $ ixSetInsert keys,
      --  bench "concurrent insert+delete" $ nfIO $ ixSetInsertDelete keys,
      --  bench "concurrent insert+select" $ nfIO $ ixSetInsertSelect keys
      -- ]
    ]

seqStmIxSetInsert keys = baseInsertSeq keys TD.mkIdxSet Ixs.insert
seqStmIxSetInsertDelete keys = baseInsertDeleteSeq keys TD.mkIdxSet Ixs.insert Ixs.remove
seqStmIxSetInsertSelect keys = baseInsertSelectSeq keys TD.mkIdxSet Ixs.insert Ixs.get

stmIxSetInsert keys = baseInsert keys TD.mkIdxSet Ixs.insert
stmIxSetInsertDelete keys = baseInsertDelete keys TD.mkIdxSet Ixs.insert Ixs.remove
stmIxSetInsertSelect keys = baseInsertSelect keys TD.mkIdxSet Ixs.insert Ixs.get


ixSetInsert keys = baseInsert keys
                      (newTVar (IXT.empty :: TD.IxEntry))
                      (\m e -> modifyTVar' m $ IXT.insert e)

ixSetInsertDelete keys = baseInsertDelete keys
                            (newTVar (IXT.empty :: TD.IxEntry))
                            (\m e -> modifyTVar' m (IXT.insert e))
                            (\m e -> modifyTVar' m (IXT.delete e))

ixSetInsertSelect keys = baseInsertDelete keys
                            (newTVar (IXT.empty :: TD.IxEntry))
                            (\m e -> modifyTVar' m (IXT.insert e))
                            (\m e -> getFromIxSet m e TD.field1)

seqIxSetInsert keys = baseInsertSeq keys
                      (newTVar (IXT.empty :: TD.IxEntry))
                      (\m e -> modifyTVar' m $ IXT.insert e)

seqIxSetInsertDelete keys = baseInsertDeleteSeq keys
                            (newTVar (IXT.empty :: TD.IxEntry))
                            (\m e -> modifyTVar' m (IXT.insert e))
                            (\m e -> modifyTVar' m (IXT.delete e))

seqIxSetInsertSelect keys = baseInsertDeleteSeq keys
                            (newTVar (IXT.empty :: TD.IxEntry))
                            (\m e -> modifyTVar' m (IXT.insert e))
                            (\m e -> getFromIxSet m e TD.field1)

getFromIxSet m e f = readTVar m >>= \x -> return $ IXT.getEQ (f e) x

baseInsertSeq keys create insert = do
  m <- atomically create
  let chunks = S.chunksOf chunkSize $ map TD.mkEntry keys
  forM_ chunks $ \c -> forM c $ \e -> atomically (insert m e)

baseInsertDeleteSeq keys create insert delete = do
  m <- atomically create
  let chunks = S.chunksOf chunkSize $ map TD.mkEntry keys
  forM_ chunks $ \c -> forM_ c $ \e -> atomically (insert m e)
  forM_ chunks $ \c -> forM_ c $ \e -> atomically (delete m e)

baseInsertSelectSeq keys create insert select = do
  m <- atomically create
  let chunks = S.chunksOf chunkSize $ map TD.mkEntry keys
  forM_ chunks $ \c -> forM_ c $ \e -> atomically (insert m e)
  replicateM_ 30 $ forM_ chunks $ \c -> forM_ c $ \e -> atomically (select m (TD.field1 e))

baseInsert keys create insert = do
  m <- atomically create
  let chunks = S.chunksOf chunkSize $ map TD.mkEntry keys
  as <- forM chunks $ \c -> async $ forM c $ \e -> atomically (insert m e)
  forM_ as wait

baseInsertDelete keys create insert delete = do
  m <- atomically create
  let chunks = S.chunksOf chunkSize $ map TD.mkEntry keys
  as <- forM chunks $ \c -> async $ forM c $ \e -> atomically (insert m e)
  forM_ as wait
  as <- forM chunks $ \c -> async $ forM c $ \e -> atomically (delete m e)
  forM_ as wait

baseInsertSelect keys create insert select = do
  m <- atomically create
  let chunks = S.chunksOf chunkSize $ map TD.mkEntry keys
  asIns <- forM chunks $ \c -> async $ forM c $ \e -> atomically (insert m e)
  replicateM_ 30 $ do
    as <- forM chunks $ \c -> async $ forM c $ \e -> atomically (select m (TD.field1 e))
    forM_ as wait
  forM_ asIns wait

bigSet keys create insert = do
  m <- atomically create
  forM_ keys $ \k -> atomically (insert m (TD.mkEntry k))
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
