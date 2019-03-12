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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

{-
  Index based on AVL-tree, it has the same functionality as Index
  but has less TVars is a balanced tree.
-}

module Data.IxSet.BalancedIndex where

import qualified Data.Set as S
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class

import qualified Data.Map as M

type Key k = (Eq k, Ord k)

data Node k v = Nob
        | Branch !k !v !(Node k v) !(Node k v) !Int
        | Joint (TVar (Node k v))

data Map k v = Map {
  content :: TVar (Node k v),
  subTreeSize :: Int
}

new :: STM (Map k v)
new = do
  n <- newTVar Nob
  pure (Map n 2)

get :: Key k => Map k v -> k -> STM (Maybe v)
get Map {..} k = get_ =<< readTVar content
 where
  get_ Nob                        = pure Nothing
  get_ (Joint t                 ) = readTVar t >>= get_
  get_ (Branch k' v left right _) = case k `compare` k' of
    LT -> get_ left
    GT -> get_ right
    EQ -> pure $ Just v

getLT :: Key k => Map k v -> k -> STM [(k, v)]
getLT Map {..} k = get_ =<< readTVar content
 where
  get_ Nob                        = pure []
  get_ (Joint t                 ) = readTVar t >>= get_
  get_ (Branch k' v left right _)
    | k' < k    = ((k', v) :) <$> nodeToList left
    | otherwise = pure []


getGT :: Key k => Map k v -> k -> STM [(k, v)]
getGT Map {..} k = get_ =<< readTVar content
 where
   get_ Nob                        = pure []
   get_ (Joint t                 ) = readTVar t >>= get_
   get_ (Branch k' v left right _)
     | k' > k    = ((k', v) :) <$> nodeToList right
     | otherwise = pure []

getBetween :: Key k => Map k v -> k -> k -> STM [(k, v)]
getBetween Map {..} k1 k2 = get_ =<< readTVar content
  where
    get_ Nob                        = pure []
    get_ (Joint t) = readTVar t >>= get_
    get_ (Branch k' v left right _)
      | k' >= k2 = pure []
      | k' <= k1 = pure []
      | otherwise = do
        l' <- get_ left
        r' <- get_ right
        pure $ (k', v) : (l' ++ r')

size :: Key k => Map k v -> STM Int
size Map {..} = size_ =<< readTVar content
 where
  size_ Nob       = pure 0
  size_ (Joint t) = readTVar t >>= size_
  size_ (Branch _ _ left right _) = do
    sl <- size_ left
    sr <- size_ right
    pure $ sl + sr + 1


nodeToList :: Node k v -> STM [(k, v)]
nodeToList Nob       = pure []
nodeToList (Joint t) = readTVar t >>= nodeToList
nodeToList (Branch k v left right _) = do
  l' <- nodeToList left
  r' <- nodeToList right
  pure $ (k, v) : (l' ++ r')

{-
  - make an accumulator to keep track of the current depth during the descent
  - reset this counter every time Tvar is matched
  - when depth > N - create a new Joint (TVar (Node))

  - replace the current subtree only if we have never crossed another Joint with Tvar in it.
    if we did -- this next found TVar will be updated instead.


-}

insert :: Key k => Map k v -> k -> v -> STM ()
insert m@(Map c mx) k v = do
  (r, _) <- updated 0 =<< readTVar c
  ifJust (writeTVar c) r
  where
    updated depth Nob = (,1) <$> mkNode depth
    updated depth b@(Branch k' v' left right height) =
      case k `compare` k' of
        LT -> do
          (left', hl) <- updated (depth + 1) left
          let height' = max (hl + 1) height
          let x = (\l -> Branch k' v' l right height') <$> left'
          pure (x, height')
        GT -> do
          (right', hr) <- updated (depth + 1) right
          let height' = max height (hr + 1)
          let x = (\r -> Branch k' v' left r height') <$> right'
          pure (x, height')
        EQ -> pure (Just (Branch k v left right height), height)
    updated _ (Joint n) = do
      (r, h) <- updated 0 =<< readTVar n
      ifJust (writeTVar n) r
      pure (Nothing, h)

    mkNode depth
      | depth < mx = pure $ Just n
      | otherwise  = Just . Joint <$> newTVar n
      where n = Branch k v Nob Nob 1

    ifJust f (Just x) = f x
    ifJust _ Nothing  = pure ()


depth :: Key k => Map k v -> STM Int
depth Map {..} = depth_ =<< readTVar content
  where
    depth_ Nob = pure 0
    depth_ (Joint n) = readTVar n >>= depth_
    depth_ (Branch _ _ _ _ d) = pure d


stats :: Map k v -> STM (Int, Int, Int)
stats Map {..} = do
  x <- readTVar content
  stats_ x
  where
    stats_ Nob                = pure (1, 0, 0)
    stats_ (Branch _ _ l r _) = do
      (nl, bl, jl) <- stats_ l
      (nr, br, jr) <- stats_ r
      pure (nl + nr, bl + br + 1, jl + jr)
    stats_ (Joint t)          = do
      x <- readTVar t
      (n, b, j) <- stats_ x
      pure (n, b, j + 1)


dumpLine :: (Show k, Show v) => Map k v -> STM String
dumpLine Map {..} = dumpLine_ =<< readTVar content
 where
  dumpLine_ Nob    = return ""
  dumpLine_ (Joint n) = do
    dl <- dumpLine_ =<< readTVar n
    pure $ "J:" ++ dl
  dumpLine_ (Branch k v left right d) = do
    l <- dumpLine_ left
    r <- dumpLine_ right
    pure
      $  "("
      ++ show k
      ++ " -> "
      ++ show v
      ++ ", ["
      ++ l
      ++ " | "
      ++ r
      ++ "])"
