{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.IxSet.STM
    (
    insert,
    remove,
    get,
    new,
    idxFun,
    ixList,
    IxSet(..),
    Idx(..)
    ) where

import GHC.Exts (Constraint)

import Data.Data
import Data.Foldable

import Control.Monad (when)

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import qualified STMContainers.Set as TS
import qualified STMContainers.Map as TM

import qualified ListT as LT

import Data.Hashable

import qualified Data.IxSet.Index as Index

type family All (c :: * -> Constraint) (xs :: [*]) :: Constraint
type instance All c '[]       = ()
type instance All c (x ': xs) = (c x, All c xs)

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
  Flat   :: !(TS.Set v)                         -> IxSet ixs v
  IdxSet :: !(TS.Set v) -> !(TList ixs (Idx v)) -> IxSet ixs v

data Idx v ix where
  Hole   :: (Eq ix)      => Idx v ix
  IdxFun :: Index.Key ix => (v -> ix) -> !(Index.TTree ix (TS.Set v)) -> Idx v ix

type AllKeys ixs = (All Eq  ixs, All Ord ixs)

ixList :: MkIxList ixs ixs a r => r
ixList = ixList' id

class MkIxList ixs ixs' v r | r -> v ixs ixs' where
  ixList' :: (TList ixs (Idx v) -> TList ixs' (Idx v)) -> r

instance MkIxList '[] ixs v (TList ixs (Idx v)) where
  ixList' acc = acc TNil

instance MkIxList ixs ixs' v r => MkIxList (ix ': ixs) ixs' v (Idx v ix -> r) where
  ixList' acc ix = ixList' (\ x -> acc (ix :-: x))

new :: AllKeys ixs =>
       (Eq v, Hashable v) =>
       TList (ixs :: [*]) (Idx v) -> STM (IxSet (ixs :: [*]) v)
new indexes = do
  s <- TS.new
  return $ IdxSet s indexes

idxFun :: Index.Key ix => (v -> ix) -> STM (Idx v ix)
idxFun f = IdxFun f <$> Index.new

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
        let k = f v
        s <- Index.get t k
        forM_ s $ \s' -> do
          TS.delete v s'
          empty <- TS.null s'
          when empty $ Index.remove t k


get :: (Index.Key i, TLookup ixs i) =>
       IxSet ixs v -> i -> STM [v]
get set i =
  case getIdx set i of
    IdxFun _ t -> Index.get t i >>= \case
        Nothing     -> return []
        Just values -> LT.toList $ TS.stream values


getIdx :: (Index.Key i, TLookup ixs i) => IxSet ixs v -> i -> Idx v i
getIdx (Flat _) _           = Hole
getIdx (IdxSet _ indexes) i = tlookup (Proxy :: Proxy i) indexes


type IdxF v = forall i . v -> Idx v i -> STM ()

class TraverseIdxs ixs v where
  traverseIdx :: v -> TList ixs (Idx v) -> IdxF v -> STM ()

instance TraverseIdxs '[] v where
  traverseIdx _ _ _ = return ()

instance {-# OVERLAPS #-} (Eq v, Hashable v, TraverseIdxs ixs v) =>
                          TraverseIdxs (i ': ixs) v where
  traverseIdx v (i :-: ixs) f = f v i >> traverseIdx v ixs f
