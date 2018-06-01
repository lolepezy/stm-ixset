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

module Tests.STMTreeMapBench where

import Control.Monad
import Control.Monad.STM

import Data.Maybe (catMaybes)
import Data.List ((\\), nub, sort)

import Data.IxSet.STMTreeMap as M

import Criterion.Main
import Criterion.Monad
import Criterion.IO

main :: IO ()
main = defaultMain [
  bgroup "stm-map" [
       bench "1"  $ whnf fib 1
     , bench "5"  $ whnf fib 5
     , bench "9"  $ whnf fib 9
     , bench "11" $ whnf fib 11
     ]
  ]
