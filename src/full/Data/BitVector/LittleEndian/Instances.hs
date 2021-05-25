-----------------------------------------------------------------------------
-- |
-- Module      :  Data.BitVector.LittleEndian.Instances
-- Copyright   :  (c) Alex Washburn 2020
-- License     :  BSD-style
--
-- Maintainer  :  github@recursion.ninja
-- Stability   :  provisional
-- Portability :  portable
--
-- Exposes the all the instances for 'BitVector'.
--
-----------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}

module Data.BitVector.LittleEndian.Instances () where

import Data.BitVector.LittleEndian.Binary          ()
import Data.BitVector.LittleEndian.MonoKeyed       ()
import Data.BitVector.LittleEndian.MonoTraversable ()
import Data.BitVector.LittleEndian.QuickCheck      ()
--import Data.BitVector.LittleEndian.TextShow        ()
