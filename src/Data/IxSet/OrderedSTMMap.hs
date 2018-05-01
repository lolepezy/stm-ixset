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

module Data.IxSet.OrderedSTMMap where

import Data.List (elem)
import Data.Set (toList, fromList)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class

type Key k = (Eq k, Ord k)

type TTree k v = TVar (Tree k v)

data Tree k v = Empty | Node !k !v !(TTree k v) !(TTree k v)
  deriving (Eq)


empty :: STM (TTree k v)
empty = newTVar Empty

insert :: Key k => TTree k v -> k -> v -> STM ()
insert t k v = readTVar t >>= \case
      Empty -> Node k v <$> empty <*> empty >>= writeTVar t
      n     -> go t n
    where
      go tn (Node key value left right)
        | k < key  = insert left  k v
        | k > key  = insert right k v
        | k == key = Node key value <$> empty <*> empty >>= writeTVar t


get :: Key k => TTree k v -> k -> STM (Maybe v)
get m k = readTVar m >>= go
  where
    go Empty = return Nothing
    go (Node key value left right)
      | k < key  = readTVar left  >>= go
      | k > key  = readTVar right >>= go
      | k == key = return $ Just value


remove :: Key k => TTree k v -> k -> STM ()
remove t k = readTVar t >>= \case
    Empty                     -> return ()
    n@(Node key _ left right) -> void (branch n)
  where
    branch n@(Node key _ left right)
      | k < key  = remove left  k
      | k == key = removeNode t left right
      | k > key  = remove right k

    removeNode tn left right =
      (,) <$> readTVar left <*> readTVar right >>= \case
        (Empty, Empty)  -> writeTVar tn Empty
        (left', Empty)  -> writeTVar tn left'
        (Empty, right') -> writeTVar tn right'
        (Node { }, Node { }) -> do
          sup <- findSucc right
          s@(Node _ _ nl nr) <- readTVar sup
          writeTVar tn s
          removeNode tn nl nr

    findSucc = go empty
      where go default_ n = readTVar n >>= \case
                  Empty                -> default_
                  Node _ _ leftChild _ -> go (return n) leftChild

    findPred = go empty
      where go default_ n = readTVar n >>= \case
                  Empty                -> default_
                  Node _ _ _ rightChild -> go (return n) rightChild
