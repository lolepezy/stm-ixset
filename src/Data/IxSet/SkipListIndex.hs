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
{-# LANGUAGE TupleSection #-}

module Data.IxSet.SkipListIndex (
  Key,
  new,
  insert,
  remove
) where

import Data.Kind

import qualified Data.Set as S
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import qualified Data.Primitive.SmallArray as SA
import qualified GHC.Exts as GE
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty( NonEmpty(..) )

type Key k = (Eq k, Ord k)

data Sorted k v = Sorted !Int [Cell k v]

data Cell k v = Single !k !v
              | Promoted !k !v !(TVar (SA.SmallArray (TVar (Sorted k v))))

data SkipList k v = SkipList {
    root           :: TVar (Sorted k v)
  , maxSubListSize :: Int
}


data SSorted (c :: Type -> Type -> Type) k v = SSorted !Int [c k v]
type USorted k v = SSorted Unit k v
type PSorted k v = SSorted Prom k v

data SList k v = SList Int [Unit k v]

data Unit k v = Actual !k !v
              | PU !(Prom k v)

data Prom k v = Prom !k !v
                     !(TVar (SSorted Unit k v))
                     !(TVar (SA.SmallArray (TVar (PSorted k v))))


data TSkipList k v = TSkipList {
    root_           :: TVar (PSorted k v)
  , maxSubListSize_ :: Int
}

defaultMaxSubListSize :: Int
defaultMaxSubListSize = 11


new :: STM (SkipList k v)
new = do
  s <- newTVar (Sorted 0 [])
  pure (SkipList s defaultMaxSubListSize)


insert :: Key k => k -> v -> SkipList k v -> STM ()
insert k v s@SkipList{..} = do
  Location section@(p, c, n) path <- locate k s
  case c of
    Just c' -> update' p n path c'
    Nothing -> insert' p n path
  where
    update' prev next path c' =
      let (pathSegment, level) = NE.last path
          replaceWith = case c' of
            Single _ _     -> Single k v
            Promoted _ _ t -> Promoted k v t
          section     = prev ++ replaceWith : next
          updatedCell = SA.indexSmallArray pathSegment level
        in
          modifyTVar' updatedCell (\(Sorted len _) -> Sorted len section)

    insert' prev next path =
      let bottomLevel = 0 :: Int
          pathSegment = NE.head $ NE.map fst $ NE.filter ((== bottomLevel) . snd) path
          section     = prev ++ Single k v : next
          updatedCell = SA.indexSmallArray pathSegment bottomLevel
      in do
        modifyTVar' updatedCell (\(Sorted len _) -> Sorted (len + 1) section)
        promote path
        where
          pathList = toList $ NE.reverse path
          promote path = go pathList
            where
              go [] = pure ()
              go ((pathSegment, level) : ps) = do
                let currentLevel = SA.indexSmallArray pathSegment level
                Sorted len section <- readTVar currentLevel
                if len >= maxSubListSize then do
                    let (previous, middle, remainder) = findMiddle len section
                    if level == 0 then
                        -- probably special case
                        pure ()
                      else do
                        middle' <- case middle of
                              s@(Single _ _)     -> promoteSingle s pathSegment
                              p@(Promoted k v t) -> promotePromoted p pathSegment
                        -- TODO Insert links to middle' into the previous layer
                        writeTVar currentLevel (Sorted (length previous + 1) (previous ++ [middle']))
                        go ps
                else
                    pure ()

                where
                  findMiddle len s =
                    let (p, m : n) = L.splitAt (len `div` 2) s
                    in (p, m, n)

                  promoteSingle (Single k v) pathSegment =  do
                    let upperLevel = SA.indexSmallArray pathSegment (level - 1)
                    currentLevel <- newTVar (Sorted (length remainder) remainder)
                    levels       <- newTVar $ GE.fromList [ upperLevel, currentLevel ]
                    return (Promoted k v levels)

                  promotePromoted p@(Promoted k v levels) pathSegment = do
                    modifyTVar' levels $ \t -> t -- TODO
                    return p


{-
   empty -> []
   +1    -> [Array[0 -> 1]]
   +2    -> [Array[0 -> 1]]         -> Single 2
   +5    -> [Array[0 -> 1]]         -> Single 2
   +5    -> [Array[0 -> 1]]         -> Single 2 -> Single 5
   +3    -> [Array[0 -> 1, 1 -> x]] -> Single 2 -> x@[Array[0 -> 3]] -> Single 5
-}

data Location k v = Location {
    _section  :: ([Unit k v], Maybe (Unit k v), [Unit k v])
  , _path     :: [(Prom k v, Int)]
}


tlocate :: Key k => k -> TSkipList k v -> STM (Maybe (Location k v))
tlocate k TSkipList{..} =
  readTVar root_ >>= \case
    (SSorted _ [])                        -> pure Nothing
    (SSorted _ promoted@(p@(Prom _ _ x h) : us)) -> do
      layers   <- readTVar h
      let currentLayer = SA.sizeofSmallArray layers - 1
      location <- getLocation promoted [(p, currentLayer)] currentLayer
      return $ Just location
  where
    getLocation :: [Prom k v] -> [(Prom k v, Int)] -> Int -> STM (Location k v)
    getLocation promoted path currentLevel =
      case currentLevel of
        0 -> get_ [] promoted currentLevel
        n -> get_ [] promoted currentLevel
      where
        get_ preceding [] currentLevel =
                pure $ Location (preceding, Nothing, []) path
        get_ preceding
             succeeding@(c : cs)
             currentLevel =
                let k' = cellKey c
                in case compare k' k of
                  EQ -> pure $ Location (preceding, Just c, cs) path
                  LT -> case c of
                    Single _ _         -> get_ (preceding ++ [c]) cs
                    p@(Promoted _ _ h) ->
                      case cs of
                        [] -> do
                          array <- readTVar h
                          let length = sizeofSmallArray array
                          if level < length
                            then go (level - 1) (SA.indexSmallArray array level)
                            else pure Nothing
                  GT -> case c of
                    Single _ _         -> pure $ Location (preceding, Nothing, succeeding) path
                    p@(Promoted _ _ a) -> ()


locate :: Key k => k -> SkipList k v -> STM (Maybe (Location k v))
locate k SkipList{..} = do
  readTVar root >>= \case
    (Sorted _ [])        -> Nothing
    (Sorted _ s@(c:cs))  -> Just <$> case c of
      Single _ _     -> nonEmptyLocation s [] 0
      Promoted _ _ h -> nonEmptyLocation s [] (sizeofSmallArray h - 1)
  where
    nonEmptyLocation sorted@(Sorted _ cells) path currentLevel =
      get_ [] cells currentLevel
      where
        get_ preceding [] currentLevel =
                pure $ Location (preceding, Nothing, []) path
        get_ preceding
             succeeding@(c : cs)
             currentLevel =
                let k' = cellKey c
                in case compare k' k of
                  EQ -> pure $ Location (preceding, Just c, cs) path
                  LT -> case c of
                    Single _ _         -> get_ (preceding ++ [c]) cs
                    p@(Promoted _ _ a) ->
                      case cs of
                        [] -> do
                          array <- readTVar h
                          let length = sizeofSmallArray array
                          if level < length
                            then go (level - 1) (SA.indexSmallArray array level)
                            else pure Nothing
                  GT -> case c of
                    Single _ _         -> pure $ Location (preceding, Nothing, succeeding) path
                    p@(Promoted _ _ a) -> ()




get :: Key k => k -> SkipList k v -> STM (Maybe v)
get k s = do
  Location (_, v, _) _ <- locate k s
  pure (cellValue <$> v)

cellValue :: Cell k v -> v
cellValue (Single _ v) = v
cellValue (Promoted _ v _) = v

cellKey :: Cell k v -> v
cellKey (Single k _) = k
cellKey (Promoted k _ _) = k

toList :: NonEmpty a -> [a]
toList (a :| as) = a : as

ifJust :: Monad m => (a -> m ()) -> Maybe a -> m ()
ifJust f s = case s of
  Nothing -> pure ()
  Just z  -> f z

remove :: Key k => k -> SkipList k v -> STM ()
remove k sl = pure ()
