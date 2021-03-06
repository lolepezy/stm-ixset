{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Tests.STMIxSetSpec where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU
import Test.QuickCheck.Monadic

import Control.Monad
import Control.Monad.STM

import Data.Hashable
import Data.Maybe (catMaybes)
import Data.List ((\\), nub, sort)

import Data.IxSet.Index as Idx
import Data.IxSet.STM (idxFun, ixList)
import qualified Data.IxSet.STM as Ixs

import GHC.Generics (Generic)

qcStmIxsProps = testGroup "STM IxSet properties"
  [
    QC.testProperty
      "Get after insert returns the same elements"
      prop_get_always_returns_inserted_elements,

    QC.testProperty
      "Get after insert returns the same elements except for removed ones"
      prop_get_always_returns_inserted_elements_except_for_removed_ones
  ]


newtype Index1 = Index1 Int deriving (Eq, Ord, Show, Generic)
newtype Index2 = Index2 Int deriving (Eq, Ord, Show, Generic)

type Entry = (Index1, String, Index2)
type EntryIdxs = '[String, Index2]

instance Arbitrary Index1 where
  arbitrary = Index1 <$> arbitrary

instance Arbitrary Index2 where
  arbitrary = Index2 <$> arbitrary

instance Hashable Index1
instance Hashable Index2

field1 :: Entry -> Index1
field1 (i, _, _) = i
field2 :: Entry -> String
field2 (_, s, _) = s
field3 :: Entry -> Index2
field3 (_, _, i) = i

mkIdxSet = do
  i1 <- idxFun field1
  i2 <- idxFun field2
  i3 <- idxFun field3
  Ixs.new $ ixList i1 i2 i3

arbitraryIxSet = do
  ixSet <- run $ atomically mkIdxSet
  entries :: [Entry] <- pick arbitrary
  run $ atomically $ forM_ entries $ Ixs.insert ixSet
  return (ixSet, entries)

prop_get_always_returns_inserted_elements :: QC.Property
prop_get_always_returns_inserted_elements = monadicIO $ do
  (ixSet, entries) <- arbitraryIxSet
  compareExtractedSets ixSet entries field1
  compareExtractedSets ixSet entries field2
  compareExtractedSets ixSet entries field3
  where
    compareExtractedSets ixSet entries fieldF = do
      subset <- nub <$> pick (sublistOf entries)
      extracted <- run $ atomically $ forM subset $ \e -> Ixs.get ixSet (fieldF e)
      assert $ all (True==) [ all (\e -> fieldF e0 == fieldF e) extr | (e0, extr) <- zip subset extracted ]


prop_get_always_returns_inserted_elements_except_for_removed_ones :: QC.Property
prop_get_always_returns_inserted_elements_except_for_removed_ones = monadicIO $ do
  (ixSet, entries) <- arbitraryIxSet

  toBeRemoved <- nub <$> pick (sublistOf entries)
  run $ atomically $ forM_ toBeRemoved $ Ixs.remove ixSet

  compareExtractedSets ixSet entries toBeRemoved field1
  compareExtractedSets ixSet entries toBeRemoved field2
  compareExtractedSets ixSet entries toBeRemoved field3
  where
    compareExtractedSets ixSet entries removed fieldF = do
      subset <- nub <$> pick (sublistOf entries)
      let leftovers = subset \\ removed
      extracted <- run $ atomically $ forM leftovers $ \e -> Ixs.get ixSet (fieldF e)
      assert $ all (True==) [ all (\e -> fieldF e0 == fieldF e) extr | (e0, extr) <- zip leftovers extracted ]

      extracted <- run $ atomically $ forM subset $ \e -> Ixs.get ixSet (fieldF e)
      assert $ null [ es | es <- extracted, any (`elem` removed) es ]
