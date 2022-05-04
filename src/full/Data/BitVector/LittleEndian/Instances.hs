{-|

Copyright   : © 2020 Alex Washburn
License     : BSD-3-Clause
Maintainer  : github@recursion.ninja
Stability   : Stable

Exposes the all the instances for 'BitVector'.

-}

{-# Language NoImplicitPrelude #-}

module Data.BitVector.LittleEndian.Instances
    (
    ) where

import Data.BitVector.LittleEndian.Binary ()
import Data.BitVector.LittleEndian.MonoKeyed ()
import Data.BitVector.LittleEndian.MonoTraversable ()
import Data.BitVector.LittleEndian.QuickCheck ()
--import Data.BitVector.LittleEndian.TextShow        ()
