{-|

Copyright   : © 2020 Alex Washburn
License     : BSD-3-Clause
Maintainer  : github@recursion.ninja
Stability   : Stable

Exposes the 'TextShow' instance for 'BitVector'.

-}

{-# Language OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.BitVector.LittleEndian.TextShow
    (
    ) where

import Data.BitVector.LittleEndian.Internal
import Data.Foldable (fold)
import TextShow (TextShow(showb))


{-| @since 1.0.0 -}
instance TextShow BitVector where

    showb (BV w n) = fold ["[", showb w, "]", showb n]
