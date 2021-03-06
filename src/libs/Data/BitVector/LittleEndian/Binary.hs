-----------------------------------------------------------------------------
-- |
-- Module      :  Data.BitVector.LittleEndian.Binary
-- Copyright   :  (c) Alex Washburn 2020
-- License     :  BSD-style
--
-- Maintainer  :  github@recursion.ninja
-- Stability   :  provisional
-- Portability :  portable
--
-- Exposes the 'Binary' instance for 'BitVector'.
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.BitVector.LittleEndian.Binary () where

import Control.Applicative
import Data.BitVector.LittleEndian.Internal
import Data.Binary


-- |
-- @since 1.2.0
instance Binary BitVector where

    put (BV w n) = put w <> put n

    get = liftA2 BV get get
