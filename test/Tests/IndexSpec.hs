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
      prop_get_always_returns_inserted_elements,

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
  --HU.testCase "Should insert and get" should_insert_and_get,
    -- HU.testCase "Should insert and get" should_insert_and_get_remove_and_dont_get
  ]

should_insert_and_get = do
  m <- atomically M.new

  let check s k = atomically (M.get m k) >>= \v -> HU.assertBool s $ v == Just ("v" ++ show k)
    in do
      atomically $ M.insert m 1 "v1"
      check "v1" 1

      atomically $ M.insert m 6 "v6"
      check "v62" 6
      check "v12" 1

      atomically $ M.insert m 0 "v0"
      check "v13" 1
      check "v03" 0
      check "v63" 6


should_insert_and_get_remove_and_dont_get = do
  m <- atomically M.new

  forM_ [-11,-12,10,-1,2,-9] $ \k -> atomically $ M.insert m k ("v" ++ show k)

  let check s k = atomically (M.get m k) >>= \v -> HU.assertBool s $ v == Just ("v" ++ show k)
    in do
      s <- atomically $ M.dumpLine m
      print $ "s0 = " ++ show s

      atomically $ M.remove m (-11)
      -- [-10,-6,-16]

      s <- atomically $ M.dumpLine m
      print $ "s = " ++ show s

      atomically $ M.remove m (10)

      s1 <- atomically $ M.dumpLine m
      print $ "s1 = " ++ show s1

      atomically $ M.remove m (1)

      s2 <- atomically $ M.dumpLine m
      print $ "s2 = " ++ show s2



prop_get_always_returns_inserted_elements :: QC.Property
prop_get_always_returns_inserted_elements = monadicIO $ do
  keys :: [Int] <- pick arbitrary
  m <- run $ atomically M.new
  run $ forM keys $ \k ->
    atomically $ M.insert m k ("v" ++ show k)

  values <- run $ forM keys $ \k ->
    atomically $ M.get m k

  s <- run $ atomically $ M.dump m
  -- run $ print $ "s = " ++ s
  assert $ [ "v" ++ show k | k <- keys] == catMaybes values


prop_get_always_returns_inserted_element_except_for_removed_ones :: QC.Property
prop_get_always_returns_inserted_element_except_for_removed_ones = monadicIO $ do
  keys :: [Int] <- nub <$> pick arbitrary
  m <- run $ atomically M.new
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
  m <- run $ atomically M.new
  run $ forM keys $ \k ->
    atomically $ M.insert m k ("v" ++ show k)

  s <- run $ atomically $ M.size m
  assert $ s == length (nub keys)


prop_getLT_returns_smaller_elements :: QC.Property
prop_getLT_returns_smaller_elements = lowerGreaterTest (<) M.getLT

prop_getGT_returns_smaller_elements :: QC.Property
prop_getGT_returns_smaller_elements = lowerGreaterTest (>) M.getGT

lowerGreaterTest :: (Int -> Int -> Bool) -> (M.Map Int String -> Int -> STM [(Int, String)]) -> QC.Property
lowerGreaterTest comparison extract = monadicIO $ do
  keys :: [Int] <- pick arbitrary
  m <- run $ atomically M.new
  run $ forM keys $ \k ->
    atomically $ M.insert m k ("v" ++ show k)

  boundary <- pick (elements keys)
  upper <- run $ atomically $ extract m boundary

  let inRange = [ k | (k, v) <- upper, k `comparison` boundary ]
  assert $ sort inRange == sort (map fst upper)


prop_getBetween_returns_proper_elements :: QC.Property
prop_getBetween_returns_proper_elements = monadicIO $ do
  keys :: [Int] <- pick (suchThat arbitrary (not . null))
  m <- run $ atomically M.new
  run $ forM keys $ \k ->
    atomically $ M.insert m k ("v" ++ show k)

  lower   <- pick (elements keys)
  greater <- pick (elements keys)
  result  <- run $ atomically $ M.getBetween m lower greater

  let inRange = [ k | (k, v) <- result, (k > lower) && (k < greater) ]
  assert $ sort inRange == sort (map fst result)
