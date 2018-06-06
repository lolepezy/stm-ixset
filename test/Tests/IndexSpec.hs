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

module Tests.IndexSpec where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU
import Test.QuickCheck.Monadic

import Control.Monad
import Control.Monad.STM

import Data.Maybe (catMaybes)
import Data.List ((\\), nub, sort)

import Data.IxSet.Index as M

import System.IO (hPutStr, stderr, stdout)

qcStmMapProps = testGroup "STM Index Map properties"
  [
    QC.testProperty
      "Get after insert returns the same element"
      prop_get_always_returns_inserted_element,

    QC.testProperty
      "Get after insert returns the same element unless it's removed explicitly"
      prop_get_always_returns_inserted_element_except_for_removed_ones,

    QC.testProperty
      "Size of the map is always the number of unique elements"
      prop_size_is_always_the_number_of_unique_elements,

    QC.testProperty
      "getLT should return keys smaller than the argument"
      prop_getLT_returns_smaller_elements,

    QC.testProperty
      "getGT should return keys bigger than the argument"
      prop_getGT_returns_smaller_elements,

    QC.testProperty
      "getBetween should return keys between the boundaries"
      prop_getBetween_returns_proper_elements
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


prop_size_is_always_the_number_of_unique_elements :: QC.Property
prop_size_is_always_the_number_of_unique_elements = monadicIO $ do
  keys :: [Int] <- pick arbitrary
  m <- run $ atomically M.empty
  run $ forM keys $ \k ->
    atomically $ M.insert m k ("v" ++ show k)

  s <- run $ atomically $ M.size m
  assert $ s == length (nub keys)


prop_getLT_returns_smaller_elements :: QC.Property
prop_getLT_returns_smaller_elements = lowerGreaterTest (<) M.getLT

prop_getGT_returns_smaller_elements :: QC.Property
prop_getGT_returns_smaller_elements = lowerGreaterTest (>) M.getGT

lowerGreaterTest :: (Int -> Int -> Bool) -> (TTree Int String -> Int -> STM [(Int, String)]) -> QC.Property
lowerGreaterTest comparison extract = monadicIO $ do
  keys :: [Int] <- pick arbitrary
  m <- run $ atomically M.empty
  run $ forM keys $ \k ->
    atomically $ M.insert m k ("v" ++ show k)

  boundary <- pick (elements keys)
  upper <- run $ atomically $ extract m boundary

  let inRange = [ k | (k, v) <- upper, k `comparison` boundary ]
  assert $ sort inRange == sort (map fst upper)


prop_getBetween_returns_proper_elements :: QC.Property
prop_getBetween_returns_proper_elements = monadicIO $ do
  keys :: [Int] <- pick (suchThat arbitrary (not . null))
  m <- run $ atomically M.empty
  run $ forM keys $ \k ->
    atomically $ M.insert m k ("v" ++ show k)

  lower   <- pick (elements keys)
  greater <- pick (elements keys)
  result  <- run $ atomically $ M.getBetween m lower greater

  let inRange = [ k | (k, v) <- result, (k > lower) && (k < greater) ]
  assert $ sort inRange == sort (map fst result)
