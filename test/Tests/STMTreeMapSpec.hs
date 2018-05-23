{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Tests.STMTreeMapSpec where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU
import Test.QuickCheck.Monadic

import Control.Monad
import Control.Monad.STM

import Data.Maybe (catMaybes)
import Data.List ((\\), nub)

import Data.IxSet.STMTreeMap as M

import System.IO (hPutStr, stderr, stdout)

qcStmMapProps = testGroup "STM Map properties"
  [
    QC.testProperty
      "Get after insert returns the same element"
      prop_get_always_returns_inserted_element,

    QC.testProperty
      "Get after insert returns the same element unless it's removed explicitly"
      prop_get_always_returns_inserted_element_except_for_removed_ones
  ]

stmMapUnitTests = testGroup "STM Map unit tests"
  [
  ]

prop_get_always_returns_inserted_element :: QC.Property
prop_get_always_returns_inserted_element = monadicIO $ do
  keys :: [Int] <- pick arbitrary
  m <- run $ atomically M.empty
  run $ forM keys $ \k ->
    atomically $ M.insert m k ("v" ++ show k)

  values <- run $ forM keys $ \k ->
    atomically $ M.get m k

  assert $ [ "v" ++ show k | k <- keys] == catMaybes values


prop_get_always_returns_inserted_element_except_for_removed_ones :: QC.Property
prop_get_always_returns_inserted_element_except_for_removed_ones = monadicIO $ do
  keys :: [Int] <- nub <$> pick arbitrary
  m <- run $ atomically M.empty
  run $ forM keys $ \k ->
    atomically $ M.insert m k ("v" ++ show k)

  toBeRemoved <- nub <$> pick (sublistOf keys)
  run $ forM toBeRemoved $ \k ->
    atomically $ M.remove m k

  values <- run $ forM keys $ \k ->
    atomically $ M.get m k

  assert $ [ "v" ++ show k | k <- keys \\ toBeRemoved] == catMaybes values
