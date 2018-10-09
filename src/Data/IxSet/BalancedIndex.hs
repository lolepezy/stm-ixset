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

newMap :: k -> v -> STM (Map k v)
newMap k v = do
  n <- newTVar (Branch k v Nob Nob 0)
  return (Map n 20)

empty :: STM (Map k v)
empty = newTVar Nob >>= \c -> pure (Map c 20)

get :: Key k => Map k v -> k -> STM (Maybe v)
get Map {..} k = get_ =<< readTVar content
 where
  get_ Nob                        = pure Nothing
  get_ (Joint t                 ) = readTVar t >>= get_
  get_ (Branch k' v left right _) = case k `compare` k' of
    LT -> get_ left
    GT -> get_ right
    EQ -> pure $ Just v



{-
  - make an accumulator to keep track of the current depth during the descent
  - reset this counter every time Tvar is matched
  - when depth > N - create a new Joint (TVar (Node))

  - replace the current subtree only if we have never crossed another Joint with Tvar in it.
    if we did -- this next found TVar will be updated instead.


-}

insert :: Key k => Map k v -> k -> v -> STM ()
insert m@(Map c mx) k v =
  readTVar c >>= updated 0 >>= ifJust (writeTVar c)
  where
    updated depth Nob                        = mkNode 0 k v
    updated depth (Branch k' _ left right _) =
      case k `compare` k' of
        LT -> updated (depth + 1) left
        GT -> updated (depth + 1) right
        EQ -> mkNode depth k v
    updated _ (Joint n) = do
      readTVar n >>= updated 0 >>= ifJust (writeTVar n)
      pure Nothing

    mkNode depth k v
      | depth < mx = pure $ Just n
      | otherwise  = Just . Joint <$> newTVar n
      where n = Branch k v Nob Nob depth

    ifJust f (Just x) = f x
    ifJust _ Nothing  = pure ()
