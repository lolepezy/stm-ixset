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
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}

module Bench.TestData where

import GHC.Generics (Generic)
import Data.Hashable
import Data.Proxy
import Data.Data

import qualified Data.IxSet.Typed as IXT

import qualified Data.IxSet.Index as Ix
import qualified Data.IxSet.STM as Ixs

newtype Index1 = Index1 Int deriving (Eq, Ord, Show, Generic, Data, Typeable)
newtype Index2 = Index2 Int deriving (Eq, Ord, Show, Generic, Data, Typeable)
newtype SField = SField String deriving (Eq, Ord, Show, Generic, Data, Typeable)

instance Hashable Index1
instance Hashable SField
instance Hashable Index2

field1 :: Entry -> Index1
field1 (i, _, _) = i
field2 :: Entry -> SField
field2 (_, s, _) = s
field3 :: Entry -> Index2
field3 (_, _, i) = i

mkEntry :: Int -> Entry
mkEntry i = (Index1 i, SField ("bla" ++ show i), Index2 (i `mod` 127))

mkIdxSet = do
  i1 <- Ixs.idxFun field1
  i2 <- Ixs.idxFun field2
  i3 <- Ixs.idxFun field3
  Ixs.new $ Ixs.ixList i1 i2 i3


-- ixset-typed stuff
type Entry = (Index1, SField, Index2)
type EntryIdxs = '[Index1, SField, Index2]

type IxEntry  = IXT.IxSet EntryIdxs Entry

instance IXT.Indexable EntryIdxs Entry where
  indices = IXT.ixList
              (IXT.ixGen (Proxy :: Proxy Index1))
              (IXT.ixGen (Proxy :: Proxy SField))
              (IXT.ixGen (Proxy :: Proxy Index2))
