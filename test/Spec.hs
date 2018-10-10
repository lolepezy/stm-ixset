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

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU
import Test.QuickCheck.Monadic

import Control.Monad.STM

import Data.IxSet.Index as M
import Tests.IndexSpec
import Tests.BalancedIndexSpec
import Tests.STMIxSetSpec

main :: IO ()
main = defaultMain $ testGroup "All tests" [
  qcStmMapProps,
  qcStmIxsProps,
  stmMapUnitTests,
  qcStmBalacedIndexProps,
  stmBalancedIndexUnitTests
  ]
