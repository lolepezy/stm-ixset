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

module Bench.STMTreeMapBench where

import Control.Monad
import Control.Monad.STM

import Data.Maybe (catMaybes)
import Data.List ((\\), nub, sort)

import Criterion.Main
import Criterion.Monad
import Criterion.IO

import qualified Data.Map as M
import qualified System.Random.MWC.Monad as MWC
import qualified Data.Text as Text
import qualified Data.Char as Char

import Data.IxSet.Index as Ix

fib x = x

rows :: Int = 100000

indexBench :: IO ()
indexBench = do
  keys <- MWC.runWithCreate $ replicateM rows keyGenerator
  defaultMain [
    bgroup "stm-map" [
        bench "simple insert" $ nfIO $ do
           t <- atomically Ix.new
           forM_ keys $ \k -> atomically $ Ix.insert t k ()
       ]
    ]

keyGenerator :: MWC.Rand IO Text.Text
keyGenerator = do
  l <- length
  s <- replicateM l char
  return $! Text.pack s
  where
    length = MWC.uniformR (7, 20)
    char = Char.chr <$> MWC.uniformR (Char.ord 'a', Char.ord 'z')
