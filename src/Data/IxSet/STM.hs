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
    remove
    ) where

import Data.Data

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import qualified STMContainers.Set as TS
import qualified STMContainers.Map as TM

import qualified ListT as LT

import qualified Data.IxSet.STMTreeMap as Index

data TList (ixs :: [*]) (f :: * -> *) where
  TNil  :: TList '[] f
  (:-:) :: f ix -> TList ixs f -> TList (ix ': ixs) f

infixr 5 :-:

class Lookup ixs ix where
  tlistLookup :: Proxy ixs -> Proxy ix -> TList ixs f -> f ix

instance Lookup (ix ': ixs) ix where
  tlistLookup _ _ (iv :-: _) = iv

instance {-# OVERLAPS #-} Lookup ixs ix => Lookup (ix1 ': ixs) ix where
  tlistLookup pList pI (i :-: ix) = tlistLookup (proxyTail pList) pI ix


data IxSet (ixs :: [*]) (v :: *) where
  Empty  :: !(TS.Set v)                         -> IxSet ixs v
  IdxSet :: !(TS.Set v) -> !(TList ixs (Idx v)) -> IxSet ixs v

data Idx v ix where
  Hole   :: (Eq ix)      => Idx v ix
  IdxFun :: Index.Key ix => (v -> ix) -> !(Index.TTree ix (TS.Set v)) -> Idx v ix


proxyTail :: Proxy (z ': zs) -> Proxy zs
proxyTail _ = Proxy


{-
  TODO Implement them
-}

insert :: IxSet ixs v -> v -> STM ()
insert _ _ = do
  
  return ()

remove :: IxSet ixs v -> v -> STM ()
remove _ _ = return ()

get :: (Index.Key i, Lookup ixs i) => IxSet ixs v -> i -> STM [v]
get set i =
    case getIdx set i of
      -- TODO make it compile time decision (through an auxilliary typeclass maybe)
      Hole       -> return []
      IdxFun _ t -> Index.get t i >>= \case
          Nothing     -> return []
          Just values -> LT.toList $ TS.stream values


getIdx :: (Index.Key i, Lookup ixs i) => IxSet ixs v -> i -> Idx v i
getIdx (Empty _) _          = Hole
getIdx (IdxSet _ indexes) i = tlistLookup Proxy (Proxy :: Proxy i) indexes



class UpdateIxds ixs v a where
  updateIdxs :: Proxy ixs -> v -> TList ixs (Idx v) -> STM a -> STM ()

instance UpdateIxds '[] v a where
  updateIdxs _ _ _ _ = return ()

instance {-# OVERLAPS #-} UpdateIxds ixs v a =>
                          UpdateIxds (i ': ixs) v a where
  updateIdxs proxies v (i :-: ix) action = do
    action
    updateIdxs (proxyTail proxies) v ix action
