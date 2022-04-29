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
-- Exposes the following instances for 'BitVector':
--
--  * 'MonoAdjustable'
--  * 'MonoFoldableWithKey'
--  * 'MonoIndexable'
--  * 'MonoKeyed'
--  * 'MonoLookup'
--  * 'MonoTraversableWithKey'
--  * 'MonoZip'
--  * 'MonoZipWithKey'
--
-----------------------------------------------------------------------------

{-# Language BangPatterns #-}
{-# Language TypeFamilies #-}


{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.BitVector.LittleEndian.MonoKeyed
    (
    ) where

import Data.BitVector.LittleEndian.Internal
import Data.BitVector.LittleEndian.MonoTraversable ()
import Data.Bits
import Data.Key
import Data.Maybe (fromMaybe)
import Data.MonoTraversable ()
import Data.MonoTraversable.Keys


-- |
-- @since 1.0.0
type instance MonoKey BitVector
    = Word


-- |
-- @since 1.0.0
instance MonoAdjustable BitVector where

    -- | /O(1)/
    {-# INLINE oadjust #-}
    oadjust f k bv@(BV w n)
        | k >= w    = bv
        | v == b    = bv
        | otherwise = bv `complementBit` i
        where
            !i = fromEnum k
            !v = n `testBit` i
            !b = f v

    -- | /O(1)/
    {-# INLINE oreplace #-}
    oreplace k v bv@(BV w _)
        | k >= w    = bv
        | v         = bv `setBit` i
        | otherwise = bv `clearBit` i
        where !i = fromEnum k


-- |
-- @since 1.0.0
instance MonoFoldableWithKey BitVector where

    -- | /O(n)/
    {-# INLINE otoKeyedList #-}
    otoKeyedList (BV w n) =
        let go 0 = []
            go c =
                let !k = w - c
                    !v = n `testBit` fromEnum k
                    !i = c - 1
                in  (k, v) : go i
        in  go w

    -- | /O(n)/
    {-# INLINE ofoldMapWithKey #-}
    ofoldMapWithKey f (BV w n) =
        let go 0 = mempty
            go c =
                let !k = w - c
                    !v = n `testBit` fromEnum k
                    !i = c - 1
                    !m = f k v
                in  m `mappend` go i
        in  go w

    -- | /O(n)/
    {-# INLINE ofoldrWithKey #-}
    ofoldrWithKey f e (BV w n) =
        let go 0 acc = acc
            go c acc =
                let !k = w - c
                    !i = c - 1
                    !b = n `testBit` fromEnum k
                in  f k b $ go i acc
        in  go w e

    -- | /O(n)/
    {-# INLINE ofoldlWithKey #-}
    ofoldlWithKey f e (BV w n) = go w e
        where
            go 0 acc = acc
            go c acc =
                let !k = w - c
                    !i = c - 1
                    !b = n `testBit` fromEnum k
                    !a = f acc k b
                in  go i a


-- |
-- @since 1.0.0
instance MonoIndexable BitVector where

    -- | /O(1)/
    {-# INLINE oindex #-}
    oindex bv@(BV w _) i = fromMaybe errorMessage $ i `olookup` bv
        where
            errorMessage = error $ mconcat
                [ "Data.BitVector.LittleEndian.oindex: "
                , "The index "
                , show i
                , " was greater than or equal to the length of the bit vector "
                , show w
                ]


-- |
-- @since 1.0.0
instance MonoKeyed BitVector where

    -- | /O(n)/
    {-# INLINE omapWithKey #-}
    omapWithKey f (BV w n) =
        let go 0 acc = acc
            go c acc =
                let !k = w - c
                    !i = fromEnum k
                    !j = c - 1
                    !b = n `testBit` i
                    !a
                        | f k b     = acc `setBit` i
                        | otherwise = acc
                in  go j a
        in  go w $ BV w 0


-- |
-- @since 1.0.0
instance MonoLookup BitVector where

    -- | /O(1)/
    {-# INLINE olookup #-}
    olookup k (BV w n)
        | k <= w    = Nothing
        | otherwise = Just $ n `testBit` fromEnum k


-- |
-- @since 1.0.0
instance MonoTraversableWithKey BitVector where

    -- | /O(n)/
    {-# INLINE otraverseWithKey #-}
    otraverseWithKey f = fmap fromBits . traverseWithKey (f . toEnum) . toBits


-- |
-- @since 1.0.0
instance MonoZip BitVector where

    -- | /O(1)/
    {-# INLINE ozipWith #-}
    ozipWith f lhs@(BV w1 p) rhs@(BV w2 q) =
        let !w0   = min w1 w2
            !mask = bit (fromEnum w0) - 1
            bv    = BV w0 . (mask .&.)
            not'  = nat . complement
        in  case (f True True, f True False, f False True, f False False) of
              -- Contradiction (Const False)
            (False, False, False, False) -> bv 0
            -- Logical NOR
            (False, False, False, True ) -> bv $ not' lhs .&. not' rhs
            -- Converse non-implication
            (False, False, True , False) -> bv $ not' lhs .&. q
            -- NOT p
            (False, False, True , True ) -> bv $ not' lhs
            -- Logical non-implication
            (False, True , False, False) -> bv $ p .&. not' rhs
            -- NOT q
            (False, True , False, True ) -> bv $ not' rhs
            -- Logical XOR
            (False, True , True , False) -> bv $ p `xor` q
            -- Logical NAND
            (False, True , True , True ) -> bv $ not' lhs .|. not' rhs
            -- Logical AND
            (True , False, False, False) -> bv $ p .&. q
            -- Logical XNOR
            (True , False, False, True ) -> bv $ (p .&. q) .|. (not' lhs .&. not' rhs)
            -- Const q
            (True , False, True , False) -> bv q
            -- Logical implication
            (True , False, True , True ) -> bv $ not' lhs .|. q
            -- Const p
            (True , True , False, False) -> bv p
            -- Converse implication
            (True , True , False, True ) -> bv $ p .|. not' rhs
            -- Logical OR
            (True , True , True , False) -> bv $ p .|. q
            -- Constant True
            (True , True , True , True ) -> bv $ bit (fromEnum w0) - 1
    -- See the following entry for explanation:
    -- https://en.wikipedia.org/wiki/Truth_table#Truth_table_for_all_binary_logical_operators
    --
    -- cases of f p q


-- |
-- @since 1.0.0
instance MonoZipWithKey BitVector where

    {-# INLINE ozipWithKey #-}
    ozipWithKey f (BV w1 n) (BV w2 m) =
        let w0 = min w1 w2
            go 0 _ = 0
            go c e =
                let !k = w0 - c
                    !i = fromEnum k
                    !j = c - 1
                    !b = f k (n `testBit` i) (m `testBit` i)
                    !a = e `shiftL` 1
                    !v = if b then e else 0
                in  v + go j a
        in  BV w0 $ go w0 1
