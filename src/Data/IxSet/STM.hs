{-# LANGUAGE ScopedTypeVariables #-}
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
    insert
    ) where

import Data.Data

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import qualified STMContainers.Set as TS
import qualified STMContainers.Map as TM

import Data.Hashable
import qualified Data.IxSet.STMTreeMap as Index

data TList (ixs :: [*]) where
  TNil  :: TList '[]
  (:-:) :: ix -> TList ixs -> TList (ix ': ixs)

infixr 5 :-:

class TListLookup ixs ix where
  tlistLookup :: Proxy ixs -> Proxy ix -> TList ixs -> ix

instance TListLookup (ix ': ixs) ix where
  tlistLookup _ _ (iv :-: _) = iv

instance {-# OVERLAPS #-} TListLookup ixs ix => TListLookup (ix1 ': ixs) ix where
  tlistLookup pList pI (i :-: ix) = tlistLookup (proxyTail pList) pI ix


data IxSet (ixs :: [*]) (v :: *) where
  IdxSet :: !(TS.Set v) -> !(TList ixs) -> IxSet ixs v

data Idx v ix where
  IdxFun :: (Eq ix, Ord ix) => (v -> ix) -> !(Index.TTree ix v) -> Idx v ix


proxyTail :: Proxy (z ': zs) -> Proxy zs
proxyTail _ = Proxy


{-
  TODO Implement them
-}

insert :: IxSet ixs v -> v -> STM ()
insert _ _ = return ()

remove :: IxSet ixs v -> v -> STM ()
remove _ _ = return ()
