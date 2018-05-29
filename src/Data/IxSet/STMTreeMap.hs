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
{-# LANGUAGE LambdaCase #-}

module Data.IxSet.STMTreeMap where

import Data.List (elem)
import qualified Data.Set as S
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
        | k == key = writeTVar t (Node key value left right)

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
        (lc@(Node kl _ _ _ ), rc@(Node kr _ _ _)) -> do
          -- TODO Use findSucc/findPred randomly
          successor        <- findSucc right
          Node ks vs nl nr <- readTVar successor
          writeTVar tn (Node ks vs left right)
          removeNode successor nl nr

    findSucc = go empty
      where go default_ n = readTVar n >>= \case
                  Empty                -> default_
                  Node _ _ leftChild _ -> go (return n) leftChild

    findPred = go empty
      where go default_ n = readTVar n >>= \case
                  Empty                -> default_
                  Node _ _ _ rightChild -> go (return n) rightChild


get :: Key k => TTree k v -> k -> STM (Maybe v)
get t k = readTVar t >>= getIt
  where
    getIt Empty = return Nothing
    getIt (Node key value left right)
      | k < key  = readTVar left  >>= getIt
      | k > key  = readTVar right >>= getIt
      | k == key = return $ Just value


getLT :: Key k => TTree k v -> k -> STM [(k, v)]
getLT t k = readTVar t >>= getIt
  where
    getIt Empty = return []
    getIt (Node key v left right)
      | key < k = ((key, v) :) <$> toList left
      | otherwise = return []


getGT :: Key k => TTree k v -> k -> STM [(k, v)]
getGT t k = readTVar t >>= getIt
  where
    getIt Empty = return []
    getIt (Node key v left right)
      | key > k = ((key, v) :) <$> toList right
      | otherwise = return []


getBetween :: Key k => TTree k v -> k -> k -> STM [(k, v)]
getBetween t k1 k2 = readTVar t >>= getIt
  where
    getIt Empty = return []
    getIt (Node key v left right)
      | key >= k2 = return []
      | key <= k1 = return []
      | otherwise = do
          l' <- getBetween left k1 k2
          r' <- getBetween right k1 k2
          return $ [(key, v)] ++ l' ++ r'


toList :: TTree k v -> STM [(k, v)]
toList t = readTVar t >>= listEm
  where
    listEm Empty = return []
    listEm (Node k v left right) = do
      l' <- listEm =<< readTVar left
      r' <- listEm =<< readTVar right
      return $ [(k, v)] ++ l' ++ r'


size :: Key k => TTree k v -> STM Int
size t = readTVar t >>= \case
  Empty               -> return 0
  Node k v left right -> do
    sl <- size left
    sr <- size right
    return $ sl + sr + 1

dump :: (Show k, Show v) => TTree k v -> STM String
dump t = readTVar t >>= go ""
  where
    go indent Empty = return ""
    go indent (Node k v left right) = do
        l' <- readTVar left
        r' <- readTVar right
        let fl = format l' indent
        let fr = format r' indent
        dl <- go (indent ++ "  ") l'
        dr <- go (indent ++ "  ") r'
        return $ show k ++ " -> " ++ show v ++
                fl ++ dl ++
                fr ++ dr

    format Empty _                        = " "
    format n@(Node k v left right) indent = "\n  " ++ indent
