{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Data.IxSet.Index where

import qualified Data.Set as S
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class

type Key k = (Eq k, Ord k)

data Node k v = Empty | Node !(TVar (Tree k v))
  deriving (Eq)

data Tree k v = Tree !k !v !(Node k v) !(Node k v)
  deriving (Eq)

type Map k v = TVar (Node k v)

new :: STM (Map k v)
new = newTVar Empty

insert :: Key k => Map k v -> k -> v -> STM ()
insert m k v = readTVar m >>= \case
    Empty -> newNode k v >>= writeTVar m
    node  -> merge node
  where
    merge (Node n) = do
      Tree key value left right <- readTVar n
      case k `compare` key of
          LT -> case left of
            Empty -> do
              ln <- newNode k v
              writeTVar n (Tree key value ln right)
            l' -> merge l'
          GT -> case right of
            Empty -> do
              rn <- newNode k v
              writeTVar n (Tree key value left rn)
            r' -> merge r'
          EQ -> writeTVar n (Tree key v left right)

remove :: Key k => Map k v -> k -> STM ()
remove m k = readTVar m >>= \case
    Empty -> return ()
    node  -> remove_ node (writeTVar m Empty)
  where
    remove_ node@(Node n) removeFromParent = do
      tree@(Tree key value left right) <- readTVar n
      case k `compare` key of
        LT -> case left of
          Empty -> return ()
          n'    -> remove_ n' (writeTVar n (Tree key value Empty right))
        GT -> case right of
          Empty -> return ()
          n'    -> remove_ n' (writeTVar n (Tree key value left Empty))
        EQ -> case (left, right) of
          (Empty, Empty)            -> removeFromParent
          (Node left', Empty)       -> readTVar left'  >>= writeTVar n
          (Empty, Node right')      -> readTVar right' >>= writeTVar n
          (Node left', Node right') ->
            if useSuccessor then
                readTVar right' >>= \case
                  Tree kr vr Empty rr -> writeTVar n (Tree kr vr left rr)
                  Tree _ _ lr _       -> do
                    (successor, succParent) <- succ' lr right'
                    Tree ks vs ls rs <- readTVar successor
                    Tree kp vp lp rp <- readTVar succParent
                    writeTVar n (Tree ks vs left right)
                    writeTVar succParent (Tree kp vp rs rp)
              else
                readTVar left' >>= \case
                  Tree kl vl ll Empty -> writeTVar n (Tree kl vl ll right)
                  Tree _ _ _ rl       -> do
                    (predessor, predParent) <- pred' rl left'
                    Tree ks vs ls rs <- readTVar predessor
                    Tree kp vp lp rp <- readTVar predParent
                    writeTVar n (Tree ks vs left right)
                    writeTVar predParent (Tree kp vp lp ls)

    -- TODO Use succ'/pred' randomly
    -- successor case
    useSuccessor = True

    succ' (Node ch) parent = readTVar ch >>= \case
      Tree _ _ Empty _ -> return (ch, parent)
      Tree _ _ left  _ -> succ' left ch

    pred' (Node ch) parent = readTVar ch >>= \case
      Tree _ _ _ Empty -> return (ch, parent)
      Tree _ _ _ right -> pred' right ch


get :: Key k => Map k v -> k -> STM (Maybe v)
get t k = get_ =<< readTVar t
  where
    get_ Empty = return Nothing
    get_ (Node n) = do
      Tree key value left right <- readTVar n
      case k `compare` key of
        LT -> get_ left
        GT -> get_ right
        EQ -> return $ Just value


getLT :: Key k => Map k v -> k -> STM [(k, v)]
getLT t k = get_ =<< readTVar t
  where
    get_ Empty = return []
    get_ (Node n) = do
      Tree k' v left right <- readTVar n
      if k' < k then ((k', v) :) <$> nodeToList left
      else return []

getGT :: Key k => Map k v -> k -> STM [(k, v)]
getGT t k = get_ =<< readTVar t
  where
    get_ Empty = return []
    get_ (Node n) = do
      Tree k' v left right <- readTVar n
      if k' > k then ((k', v) :) <$> nodeToList right
      else return []

getBetween :: Key k => Map k v -> k -> k -> STM [(k, v)]
getBetween t k1 k2 = get_ =<< readTVar t
  where
    get_ Empty = return []
    get_ (Node n) = go =<< readTVar n
    go (Tree key value left right)
      | key >= k2 = return []
      | key <= k1 = return []
      | otherwise = do
        l' <- get_ left
        r' <- get_ right
        return $ (key, value) : (l' ++ r')

toList :: Map k v -> STM [(k, v)]
toList t = nodeToList =<< readTVar t

nodeToList :: Node k v -> STM [(k, v)]
nodeToList Empty = return []
nodeToList (Node n) = do
    Tree k v left right <- readTVar n
    l' <- nodeToList left
    r' <- nodeToList right
    return $ (k, v) : (l' ++ r')

size :: Key k => Map k v -> STM Int
size t = size_ =<< readTVar t
  where
  size_ Empty = return 0
  size_ (Node n) = do
    Tree _ _ left right <- readTVar n
    sl <- size_ left
    sr <- size_ right
    return $ sl + sr + 1


isEmpty :: Key k => Map k v -> STM Bool
isEmpty t = readTVar t >>= \case
  Empty -> return True
  _     -> return False

dump :: (Show k, Show v) => Map k v -> STM String
dump t = go "" =<< readTVar t
  where
    go indent Empty = return ""
    go indent (Node n) = do
        Tree k v left right <- readTVar n
        fl <- format left  indent
        fr <- format right indent
        dl <- go (indent ++ "  ") left
        dr <- go (indent ++ "  ") right
        return $ show k ++ " -> " ++ show v ++
                fl ++ dl ++
                fr ++ dr

    format Empty _  = return " "
    format (Node n) indent = do
      Tree k v left right <- readTVar n
      return $ "\n  " ++ indent

dumpLine :: (Show k, Show v) => Map k v -> STM String
dumpLine t = dumpLine_ =<< readTVar t
  where
    dumpLine_ Empty    = return ""
    dumpLine_ (Node n) = do
        Tree k v left right <- readTVar n
        l <- dumpLine_ left
        r <- dumpLine_ right
        return $ "(" ++ show k ++ " -> " ++ show v ++ ", [" ++ l ++ " | " ++ r ++ "])"

newNode k v = Node <$> newTVar (Tree k v Empty Empty)
