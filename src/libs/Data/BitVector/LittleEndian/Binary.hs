{-|

Copyright   : © 2020 Alex Washburn
License     : BSD-3-Clause
Maintainer  : github@recursion.ninja
Stability   : Stable

Exposes the 'Binary' instance for 'BitVector'.

-}

{-# Language Safe #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.BitVector.LittleEndian.Binary
    (
    ) where

import Control.Applicative
import Data.Binary
import Data.BitVector.LittleEndian.Internal


{-| @since 1.2.0 -}
instance Binary BitVector where

    put (BV w n) = put w <> put n

    get = liftA2 BV get get
