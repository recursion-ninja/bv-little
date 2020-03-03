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
-- Exposes the 'TextShow' instance for 'BitVector'.
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.BitVector.LittleEndian.TextShow () where

import Data.BitVector.LittleEndian.Internal
import TextShow (TextShow(showb))


-- |
-- @since 1.0.0
instance TextShow BitVector where

    showb (BV w n) = mconcat [ "[", showb w, "]", showb n ]
