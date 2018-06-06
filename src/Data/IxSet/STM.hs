{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Data.IxSet.STM
    (
    insert,
    remove,
    get
    ) where

import Data.Data
import Data.Foldable

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import qualified STMContainers.Set as TS
import qualified STMContainers.Map as TM

import qualified ListT as LT

import Data.Hashable

import qualified Data.IxSet.Index as Index

data TList (ixs :: [*]) (f :: * -> *) where
  TNil  :: TList '[] f
  (:-:) :: f i -> TList ixs f -> TList (i ': ixs) f

infixr 5 :-:

class TLookup ixs i where
  tlookup :: Proxy i -> TList ixs f -> f i

instance TLookup (i ': ixs) i where
  tlookup _ (iv :-: _) = iv

instance {-# OVERLAPS #-} TLookup ixs i => TLookup (i1 ': ixs) i where
  tlookup i' (i :-: ix) = tlookup i' ix


data IxSet (ixs :: [*]) (v :: *) where
  Empty  :: !(TS.Set v)                         -> IxSet ixs v
  IdxSet :: !(TS.Set v) -> !(TList ixs (Idx v)) -> IxSet ixs v

data Idx v ix where
  Hole   :: (Eq ix)      => Idx v ix
  IdxFun :: Index.Key ix => (v -> ix) -> !(Index.TTree ix (TS.Set v)) -> Idx v ix


proxyTail :: Proxy (z ': zs) -> Proxy zs
proxyTail _ = Proxy


insert :: (Eq v, Hashable v) =>
          TraverseIdxs ixs v =>
          IxSet ixs v -> v -> STM ()
insert (IdxSet set indexes) v = do
  TS.insert v set
  traverseIdx v indexes $ \v i ->
    case i of
      Hole       -> return ()
      IdxFun f t -> do
        let k  = f v
        Index.get t k >>= \case
          Nothing -> do
            s <- TS.new
            TS.insert v s
            Index.insert t k s
          Just s  -> TS.insert v s


remove :: (Eq v, Hashable v) =>
          TraverseIdxs ixs v =>
          IxSet ixs v -> v -> STM ()
remove (IdxSet set indexes) v = do
  TS.delete v set
  traverseIdx v indexes $ \v i ->
    case i of
      Hole       -> return ()
      IdxFun f t -> do
        s <- Index.get t (f v)
        forM_ s (TS.delete v)


get :: (Index.Key i, TLookup ixs i) =>
       IxSet ixs v -> i -> STM [v]
get set i =
  case getIdx set i of
    -- TODO make it compile time decision (through an auxilliary typeclass maybe)
    Hole       -> return []
    IdxFun _ t -> Index.get t i >>= \case
        Nothing     -> return []
        Just values -> LT.toList $ TS.stream values


getIdx :: (Index.Key i, TLookup ixs i) => IxSet ixs v -> i -> Idx v i
getIdx (Empty _) _          = Hole
getIdx (IdxSet _ indexes) i = tlookup (Proxy :: Proxy i) indexes


type IdxF v = forall i . v -> Idx v i -> STM ()

class TraverseIdxs ixs v where
  traverseIdx :: v -> TList ixs (Idx v) -> IdxF v -> STM ()

instance TraverseIdxs '[] v where
  traverseIdx _ _ _ = return ()

instance {-# OVERLAPS #-} (Eq v, Hashable v, TraverseIdxs ixs v) =>
                          TraverseIdxs (i ': ixs) v where
  traverseIdx v (i :-: ixs) f = f v i >> traverseIdx v ixs f
