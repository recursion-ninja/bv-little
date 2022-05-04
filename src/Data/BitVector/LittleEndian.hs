{-|

Copyright   : © 2020 Alex Washburn
License     : BSD-3-Clause
Maintainer  : github@recursion.ninja
Stability   : Stable

A bit vector similar to @Data.BitVector@ from the
<https://hackage.haskell.org/package/bv bv>, however the endianness is
reversed. This module defines /little-endian/ pseudo–size-polymorphic
bit vectors.

Little-endian bit vectors are isomorphic to a @[Bool]@ with the /least/
significant bit at the head of the list and the /most/ significant bit at the
end of the list. Consequently, the endianness of a bit vector affects the semantics
of many type-classes that have a linear ordering.

For an implementation of bit vectors which are isomorphic to a @[Bool]@ with the /most/
significant bit at the head of the list and the /least/ significant bit at the
end of the list, use the
<https://hackage.haskell.org/package/bv bv> package.

This module does /not/ define numeric instances for 'BitVector'. This is
intentional! To interact with a bit vector as an 'Integral' value,
convert the 'BitVector' using either 'toSignedNumber' or 'toUnsignedNumber'.

This module defines 'rank' and 'select' operations for 'BitVector' as a
<https://en.wikipedia.org/wiki/Succinct_data_structure succinct data structure>.
These operations are not /o(1)/ so 'BitVector' is not a /true/ succinct data
structure. However, it could potentially be extend to support this in the
future.
-}

{-# Language Safe #-}

module Data.BitVector.LittleEndian
    ( BitVector ()
      -- * Bit-stream conversion
    , fromBits
    , toBits
      -- * Numeric conversion
    , fromNumber
    , toSignedNumber
    , toUnsignedNumber
      -- * Queries
    , dimension
    , isZeroVector
    , subRange
      -- * Rank / Select
    , rank
    , select
    ) where

import Data.BitVector.LittleEndian.Internal
