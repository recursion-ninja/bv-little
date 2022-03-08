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
--  * 'MonoFoldable'
--  * 'MonoFunctor'
--  * 'MonoTraversable'
--
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.BitVector.LittleEndian.MonoTraversable () where

import Data.Bits
import Data.BitVector.LittleEndian.Internal
import Data.Foldable
import Data.Monoid        ()
import Data.MonoTraversable


-- |
-- @since 0.1.0
type instance Element BitVector = Bool


-- |
-- @since 0.1.0
instance MonoFoldable BitVector where

    {-# INLINE ofoldMap #-}
    ofoldMap f (BV w n) = go m
      where
        !m = fromEnum w
        go 0 = mempty
        go c = let !i = m - c
                   !j = c - 1
                   !b = n `testBit` i
               in  f b `mappend` go j
                      
    {-# INLINE ofoldr #-}
    ofoldr f e (BV w n) =
      let !m = fromEnum w
          go 0 acc = acc
          go c acc = let !i = m - c
                         !j = c - 1
                         !b = n `testBit` i
                     in  f b $ go j acc
      in  go m e

    {-# INLINE ofoldl' #-}
    ofoldl' f e (BV w n) = go m e
      where
        !m = fromEnum w
        go 0 acc = acc
        go c acc = let !i = m - c
                       !j = c - 1
                       !b = n `testBit` i
                       !a = f acc b
                   in  go j a

    {-# INLINE otoList #-}
    otoList = toBits

    -- | /O(1)/
    {-# INLINE oall #-}
    oall _ (BV 0 _) = True
    oall f (BV w n) =
        case (f False, f True) of
          (False, False) -> False
          (True , True ) -> True
          (False, True ) -> n == bit (fromEnum w) - 1
          (True , False) -> n == 0

    -- | /O(1)/
    {-# INLINE oany #-}
    oany _ (BV 0 _) = False
    oany f (BV w n) =
        case (f False, f True) of
          (False, False) -> False
          (True , True ) -> True
          (False, True ) -> n > 0
          (True , False) -> n < bit (fromEnum w) - 1

    -- | /O(1)/
    {-# INLINE onull #-}
    onull   = (== 0) . dim

    -- | /O(1)/
    {-# INLINE olength #-}
    olength = fromEnum . dim

    -- | /O(1)/
    {-# INLINE olength64 #-}
    olength64 = toEnum . olength

    {-# INLINE otraverse_ #-}
    otraverse_ f (BV w n) = go (fromEnum w) 
      where
        go 0 = pure ()
        go c = let !j = c - 1
                   !a = f (n `testBit` j)
               in  a *> go j


    {-# INLINE ofoldlM #-}
    ofoldlM f e (BV w n) = go (fromEnum w) e
      where
        go 0 acc = pure acc
        go c acc = let !j = c - 1
                       !x = f acc (n `testBit` j)
                   in  x >>= go j
        
    {-# INLINE ofoldMap1Ex #-}
    ofoldMap1Ex _ (BV 0 _) = Prelude.error "Data.MonoTraversable.ofoldMap1Ex on an empty BitVector!"
    ofoldMap1Ex f (BV w n) = go 0
      where
        !m    = fromEnum w
        go !c
          | c >= m - 1 = f $ n `testBit` c
          | otherwise  = let !j = c + 1
                             !b = n `testBit` c
                         in  f b <> go j

    -- | /O(1)/
    {-# INLINE ofoldr1Ex #-}
    ofoldr1Ex _    (BV 0 _) = Prelude.error "Data.MonoTraversable.ofoldr1Ex on an empty BitVector!"
    ofoldr1Ex _    (BV 1 n) = n > 0
    ofoldr1Ex f bv@(BV w n) =
        -- See the following entry for explanation:
        -- https://en.wikipedia.org/wiki/Truth_table#Truth_table_for_all_binary_logical_operators
        --
        -- cases of f p q
        case (f True True, f True False, f False True, f False False) of
          -- Contradiction (Const False)
          (False, False, False, False) -> False
          -- Logical NOR
          (False, False, False, True ) -> let !lzs = toEnum $ countLeadingZeros bv
                                          in  if (w - lzs) == 1 || n == 0
                                              then even lzs
                                              else odd  lzs
          -- Converse non-implication
          --   Only True when of the form <0+1>
          (False, False, True , False) -> n == bit (fromEnum w - 1)
          -- NOT p
          (False, False, True , True ) -> not (n `testBit` 0)
          -- Logical non-implication
          --   Only True when the number of leading ones is even
          (False, True , False, False) -> let !los = countLeadingZeros $ complement bv
                                          in  odd los
          -- NOT q
          (False, True , False, True ) -> let !v = n `testBit` (fromEnum w - 1)
                                          in  if even w then not v else v
          -- Logical XOR
          (False, True , True , False) -> odd $ popCount n
          -- Logical NAND
          (False, True , True , True ) -> let !los = countLeadingZeros $ complement bv
                                              !x   = bit (fromEnum w - 1) - 1
                                              !y   = bit (fromEnum w    ) - 1
                                          in  if n == x || n == y
                                              then odd  los
                                              else even los
          -- Logical AND
          (True , False, False, False) -> n == bit (fromEnum w) - 1
          -- Logical XNOR
          (True , False, False, True ) -> let !pc = popCount n
                                          in  if   even w
                                              then even pc
                                              else odd  pc
          -- Const q
          (True , False, True , False) -> n `testBit` (fromEnum w - 1)
          -- Logical implication
          --   only False when of the form <1+0>
          (True , False, True , True ) -> let !i = fromEnum w - 1
                                          in  n /= bit i - 1
          -- Const p
          (True , True , False, False) -> n `testBit` 0
          -- Converse implication
          (True , True , False, True ) -> even $ countLeadingZeros bv
          -- Logical OR
          (True , True , True , False) -> n > 0
          -- Constant True
          (True , True , True , True ) -> True

    -- | /O(n)/
    {-# INLINE ofoldl1Ex' #-}
    ofoldl1Ex' _    (BV 0 _) = Prelude.error "Data.MonoTraversable.ofoldl1Ex' on an empty BitVector!"
    ofoldl1Ex' _    (BV 1 n) = n > 0
    ofoldl1Ex' f bv@(BV w n) =
        -- See the following entry for explanation:
        -- https://en.wikipedia.org/wiki/Truth_table#Truth_table_for_all_binary_logical_operators
        --
        -- cases of f p q
        case (f True True, f True False, f False True, f False False) of
          -- Contradiction (Const False)
          (False, False, False, False) -> False
          -- Logical NOR
          (False, False, False, True ) -> let !tzs = toEnum $ countTrailingZeros bv
                                          in  if (w - tzs) == 1 || n == 0
                                              then even tzs
                                              else odd  tzs
          -- Converse non-implication
          (False, False, True , False) -> let !tzs = countTrailingZeros $ complement bv
                                          in  odd tzs
          -- NOT p
          (False, False, True , True ) -> even w == even n
          -- Logical non-implication
          (False, True , False, False) -> n == 1
          -- NOT q
          (False, True , False, True ) -> not $ n `testBit` (fromEnum w - 1)
          -- Logical XOR
          (False, True , True , False) -> odd  $ popCount n
          -- Logical NAND
          (False, True , True , True ) -> let !tos = countTrailingZeros $ complement bv
                                              !x   = bit (fromEnum w) - 1
                                              !y   = bit (fromEnum w) - 2
                                          in  if n == x || n == y
                                              then odd  tos
                                              else even tos
          -- Logical AND
          (True , False, False, False) -> n == bit (fromEnum w) - 1
          -- Logical XNOR
          (True , False, False, True ) -> let !pc = popCount n
                                          in  if   even w
                                              then even pc
                                              else odd  pc
          -- Const q
          (True , False, True , False) -> n `testBit` (fromEnum w - 1)
          -- Logical implication
          (True , False, True , True ) -> even $ countTrailingZeros bv
          -- Const p
          (True , True , False, False) -> n `testBit` 0
          -- Converse implication
          --    only False when of the form <01+>
          (True , True , False, True ) -> n /= bit (fromEnum w) - 2
          -- Logical OR
          (True , True , True , False) -> n > 0
          -- Constant True
          (True , True , True , True ) -> True

    -- | /O(1)/
    {-# INLINE headEx #-}
    headEx (BV 0 _) = error "Call to Data.MonoFoldable.headEx on an empty BitVector!"
    headEx (BV _ n) = n `testBit` 0

    -- | /O(1)/
    {-# INLINE lastEx #-}
    lastEx (BV 0 _) = error "Call to Data.MonoFoldable.lastEx on an empty BitVector!"
    lastEx (BV w n) = n `testBit` (fromEnum w - 1)

    -- | /O(n)/
    {-# INLINE maximumByEx #-}
    maximumByEx _ (BV 0 _) = error "Call to Data.MonoFoldable.maximumByEx on an empty BitVector!"
    maximumByEx _ (BV 1 n) = n /= 0
    maximumByEx f  bv      = maximumBy f $ toBits bv

    -- | /O(n)/
    {-# INLINE minimumByEx #-}
    minimumByEx _ (BV 0 _) = error "Call to Data.MonoFoldable.minimumByEx on an empty BitVector!"
    minimumByEx _ (BV 1 n) = n /= 0
    minimumByEx f  bv      = minimumBy f $ toBits bv

    -- | /O(1)/
    {-# INLINE oelem #-}
    oelem _     (BV 0 _) = False
    oelem True  (BV _ n) = n > 0
    oelem False (BV w n) = n < bit (fromEnum w) - 1

    -- | /O(1)/
    {-# INLINE onotElem #-}
    onotElem e = not . oelem e


-- |
-- @since 0.1.0
instance MonoFunctor BitVector where

    -- | /O(1)/
    {-# INLINE omap #-}
    omap f bv@(BV w n) =
        case (f False, f True) of
          (False, False) -> BV w 0
          (True , True ) -> BV w $ bit (fromEnum w) - 1
          (False, True ) -> bv
          (True , False) -> let !allOnes = bit (fromEnum w) - 1
                            in  BV w $ n `xor` allOnes


-- |
-- @since 0.1.0
instance MonoTraversable BitVector where

    -- | /O(n)/
    {-# INLINE otraverse #-}
    otraverse f = fmap fromBits . traverse f . toBits

    -- | /O(n)/
    {-# INLINE omapM #-}
    omapM = otraverse
