-----------------------------------------------------------------------------
-- |
-- Module      :  Data.BitVector.LittleEndian
-- Copyright   :  (c) Alex Washburn 2018
-- License     :  BSD-style
--
-- Maintainer  :  github@recursion.ninja
-- Stability   :  provisional
-- Portability :  portable
--
-- A bit vector similar to @Data.BitVector@ from the
-- <https://hackage.haskell.org/package/bv bv>, however the endianness is
-- reversed. This module defines /little-endian/ pseudo–size-polymorphic
-- bit vectors.
--
-- Little-endian bit vectors are isomorphic to a @[Bool]@ with the /least/
-- significant bit at the head of the list and the /most/ significant bit at the
-- end of the list. Consequently, the endianness of a bit vector affects the semantics of the
-- following typeclasses:
--
--   * 'Bits'
--   * 'FiniteBits'
--   * 'Semigroup'
--   * 'Monoid'
--   * 'MonoFoldable'
--   * 'MonoTraversable'
--
-- For an implementation of bit vectors which are isomorphic to a @[Bool]@ with the /most/
-- significant bit at the head of the list and the /least/ significant bit at the
-- end of the list, use the
-- <https://hackage.haskell.org/package/bv bv> package.
--
-- This module does /not/ define numeric instances for 'BitVector'. This is
-- intentional! To interact with a bit vector as an 'Integral' value,
-- convert the 'BitVector' using either 'toSignedNumber' or 'toUnsignedNumber'.
--
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, MagicHash #-}
{-# LANGUAGE Trustworthy, TypeFamilies #-}

module Data.BitVector.LittleEndian
  ( BitVector()
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
  ) where


import Control.DeepSeq
import Data.Bits
import Data.Data
import Data.Foldable
import Data.Hashable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid        ()
import Data.MonoTraversable
import Data.Ord
import Data.Primitive.ByteArray
import Data.Semigroup
import Data.Word
import GHC.Exts
import GHC.Generics
import GHC.Integer.GMP.Internals
import GHC.Integer.Logarithms
import GHC.Natural
import Test.QuickCheck (Arbitrary(..), CoArbitrary(..), NonNegative(..), suchThat, variant)


-- |
-- A little-endian bit vector of non-negative dimension.
data  BitVector
    = BV
    { dim :: {-# UNPACK #-} !Word -- ^ The /dimension/ of a bit vector.
    , nat :: !Natural             -- ^ The value of a bit vector, as a natural number.
    } deriving ( Data
               , Generic
               , Typeable
               )


-- |
-- /Since: 0.1.0.0/
type instance Element BitVector = Bool


-- |
-- /Since: 0.1.0.0/
instance Arbitrary BitVector where

    arbitrary = do
        dimVal <- getNonNegative <$> arbitrary
        let upperBound = shiftL 1 dimVal
        intVal <- (getNonNegative <$> arbitrary) `suchThat` (< upperBound)
        pure . BV (toEnum dimVal) $ intToNat intVal


-- |
-- /Since: 0.1.0.0/
instance Bits BitVector where

    {-# INLINE (.&.) #-}
    (BV w1 a) .&. (BV w2 b) = BV (max w1 w2) $ a .&. b

    {-# INLINE (.|.) #-}
    (BV w1 a) .|. (BV w2 b) = BV (max w1 w2) $ a .|. b

    {-# INLINE xor #-}
    (BV w1 a) `xor` (BV w2 b) = BV (max w1 w2) $ a `xor` b

    {-# INLINE complement #-}
    complement (BV w n) = BV w $ (shiftL 1 (fromEnum w)) - 1 - n

    {-# INLINE zeroBits #-}
    zeroBits = BV 0 0

    {-# INLINE bit #-}
    bit i = BV (succ $ toEnum i)  (shiftL 1 i)

    {-# INLINE clearBit #-}
    -- We do this more complicated operation rather than call 'clearBit'
    -- because it is undefined for Natural in base < 4.10.0.0
    clearBit bv@(BV w n) i
      | i < 0 || toEnum i >= w = bv
      | otherwise =
        let !allBits = pred . shiftL 1 $ fromEnum w
            !mask    = bit i `xor` allBits
        in  BV w $ n .&. mask

{-
    {-# INLINE setBit #-}
    setBit bv@(BV w n) i
      | i < 0 || i >= w = bv
      | otherwise       = BV w $ n `setBit` i
-}

    {-# INLINE testBit #-}
    testBit (BV w n) i = i >= 0 && toEnum i < w && n `testBit` i

    bitSize (BV w _) = fromEnum w

    {-# INLINE bitSizeMaybe #-}
    bitSizeMaybe (BV w _) = Just $ fromEnum w

    {-# INLINE isSigned #-}
    isSigned = const False

    {-# INLINE shiftL #-}
    shiftL (BV w n) k
      | toEnum k > w = BV w 0
      | otherwise    = BV w $ shiftL n k .&. pred (shiftL 1 (fromEnum w))

    {-# INLINE shiftR #-}
    shiftR (BV w n) k
      | toEnum k > w = BV w 0
      | otherwise    = BV w $ shiftR n k

    {-# INLINE rotateL #-}
    rotateL bv          0 = bv
    rotateL bv@(BV 0 _) _ = bv
    rotateL bv@(BV 1 _) _ = bv
    rotateL bv@(BV w n) k
      | k <  0    = bv
      | j >= w    = go . fromEnum $ j `mod` w
      | otherwise = go k
      where
        !j     = toEnum k
        go  0  = bv
        go !i  = BV w $ h + l
          where
            !v = fromEnum w
            !d = v - i
            !m = pred $ shiftL 1 d
            !l = n `shiftR` d
            !h = (n .&. m) `shiftL` i

    {-# INLINE rotateR #-}
    rotateR bv          0 = bv
    rotateR bv@(BV 0 _) _ = bv
    rotateR bv@(BV 1 _) _ = bv
    rotateR bv@(BV w n) k
      | k <  0    = bv
      | j >= w    = go . fromEnum $ j `mod` w
      | otherwise = go k
      where
        !j     = toEnum k
        go  0  = bv
        go !i  = BV w $ h + l
          where
            !v = fromEnum w
            !d = v - i
            !m = pred $ shiftL 1 i
            !l = n `shiftR` i
            !h = (n .&. m) `shiftL` d

    {-# INLINE popCount #-}
    popCount = popCount . nat


-- |
-- /Since: 0.1.0.0/
instance CoArbitrary BitVector where

    coarbitrary bv = variant (dimension bv)


-- |
-- /Since: 0.1.0.0/
instance Eq BitVector where

    {-# INLINE (==) #-}
    (==) (BV w1 m) (BV w2 n) = w1 == w2 && m == n


-- |
-- /Since: 0.1.0.0/
instance FiniteBits BitVector where

    {-# INLINE finiteBitSize #-}
    finiteBitSize = fromEnum . dim

    {-# INLINE countTrailingZeros #-}
    countTrailingZeros (BV w n) = max 0 $ fromEnum w - lastSetBit - 1
      where
        lastSetBit = I# (integerLog2# (toInteger n))

    {-# INLINE countLeadingZeros #-}
    countLeadingZeros (BV w      0) = fromEnum w
    countLeadingZeros (BV w natVal) =
        case natVal of
          NatS#      v  -> countTrailingZeros $ iMask .|. W# v
          NatJ# (BN# v) -> f $ ByteArray v
      where
        iMask = complement zeroBits `xor` (2 ^ w - 1)
        !x = fromEnum w

        f :: ByteArray -> Int
        f byteArr = g 0
          where
            (q, r) = x `quotRem` 64
            wMask  = complement zeroBits `xor` (2 ^ r - 1) :: Word64

            g :: Int -> Int
            g !i
              | i >= q = countTrailingZeros $ wMask .|. value
              | otherwise =
                  case countTrailingZeros value of
                    64 -> 64 + g (i+1)
                    v  -> v
              where
                value :: Word64
                value = byteArr `indexByteArray` i


-- |
-- /Since: 0.1.0.0/
instance Hashable BitVector where

    hash (BV w n) = fromEnum w `hashWithSalt` hash n

    hashWithSalt salt bv = salt `hashWithSalt` hash bv


-- |
-- /Since: 0.1.0.0/
instance Monoid BitVector where

    {-# INLINE mappend #-}
    mappend = (<>)

    {-# INLINE mconcat #-}
    mconcat bs =
        case bs of
          []   -> mempty
          x:xs -> sconcat $ x:|xs

    {-# INLINE mempty #-}
    mempty = BV 0 0


-- |
-- /Since: 0.1.0.0/
instance MonoFoldable BitVector where

    {-# INLINE ofoldMap #-}
    ofoldMap f (BV w n) = go m
      where
        !m = fromEnum w
        go  0 = mempty
        go !c = let !i = m - c
                    !j = c - 1
                    !b = n `testBit` i
                in  f b `mappend` go j
                      
    {-# INLINE ofoldr #-}
    ofoldr f e (BV w n) =
      let !m = fromEnum w
          go  0 acc = acc
          go !c acc = let !i = m - c
                          !j = c - 1
                          !b = n `testBit` i
                      in  f b $ go j acc
      in  go m e

    {-# INLINE ofoldl' #-}
    ofoldl' f e (BV w n) = go m e
      where
        !m = fromEnum w
        go  0 acc = acc
        go !c acc = let !i = m - c
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
        go !c = let !j = c - 1
                    !a = f (n `testBit` j)
                in  a *> go j


    {-# INLINE ofoldlM #-}
    ofoldlM f e (BV w n) = go (fromEnum w) e
      where
        go  0 acc = pure acc
        go !c acc = let !j = c - 1
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
{-                    
    ofoldMap1Ex f (BV w n) = go m
      where
        !m    = fromEnum w
        go  1 = f $ n `testBit` 0
        go !c = let !i = m - c
                    !j = c - 1
                    !b = n `testBit` i
                in  f b <> go j
-}

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
          (False, False, False, True ) -> let !lz = toEnum $ countLeadingZeros bv
                                          in  if (w - lz) == 1 || n == 0
                                              then even lz
                                              else odd lz
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
          (False, True , True , False) -> odd  $ popCount n
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
    -- TODO: Optimize this like ofoldl1Ex'
    ofoldl1Ex' _ (BV 0 _) = Prelude.error "Data.MonoTraversable.ofoldl1Ex' on an empty BitVector!"
    ofoldl1Ex' f bv = ofoldl1Ex' f $ toBits bv

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
{-    
    maximumByEx f (BV w n) =
        let !allBits  = bit (fromEnum w) - 1
            !anyFalse = n < allBits
            !anyTrue  = n > 0
        in  case f False False of
              LT -> n `testBit` ((fromEnum w) - 1)
              GT -> anyFalse
              _  -> case f True True of
                      GT -> anyTrue
                      LT -> anyFalse
                      _  -> if   n == allBits
                            then True  -- All bits on,  max value is true
                            else if n == 0
                            then False -- All bits off, max value is false
                            else case (f False True, f True False) of
                                   (LT, LT) -> True 
                                   (LT, EQ) -> True
                                   (LT, GT) -> anyTrue
                                   (EQ, LT) -> False
                                   (EQ, EQ) -> True
                                   (EQ, GT) -> True
                                   (GT, LT) -> not anyFalse
                                   (GT, EQ) -> False
                                   (GT, GT) -> False
-}

    -- | /O(n)/
    {-# INLINE minimumByEx #-}
    minimumByEx _ (BV 0 _) = error "Call to Data.MonoFoldable.minimumByEx on an empty BitVector!"
    minimumByEx _ (BV 1 n) = n /= 0
    minimumByEx f  bv      = minimumBy f $ toBits bv
{-
    minimumByEx f (BV w n) =
        let !allBits  = bit (fromEnum w) - 1
            !anyFalse = n < allBits
            !anyTrue  = n > 0
        in  case f False False of
              LT -> anyFalse
              _  -> case f True True of
                      LT -> anyTrue
                      _  -> if   n == allBits
                            then True  -- All bits on,  min value is true
                            else if n == 0
                            then False -- All bits off, min value is false
                            else case (f False True, f True False) of
                                   (LT, LT) -> False
                                   (LT, EQ) -> False
                                   (LT, GT) -> not anyFalse
                                   (EQ, LT) -> True
                                   (EQ, EQ) -> False
                                   (EQ, GT) -> False
                                   (GT, LT) -> anyTrue
                                   (GT, EQ) -> True
                                   (GT, GT) -> True
-}

    -- | /O(1)/
    {-# INLINE oelem #-}
    oelem _     (BV 0 _) = False
    oelem True  (BV _ n) = n > 0
    oelem False (BV w n) = n < bit (fromEnum w) - 1

    -- | /O(1)/
    {-# INLINE onotElem #-}
    onotElem e = not . oelem e


-- |
-- /Since: 0.1.0.0/
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
-- /Since: 0.1.0.0/
instance MonoTraversable BitVector where

    {-# INLINE otraverse #-}
    otraverse f = fmap fromBits . traverse f . toBits

    {-# INLINE omapM #-}
    omapM = otraverse


-- |
-- /Since: 0.1.0.0/
instance NFData BitVector where

    -- Already a strict data type,
    -- always in normal form.
    {-# INLINE rnf #-}
    rnf = const ()


-- |
-- /Since: 0.1.0.0/
instance Ord BitVector where

    {-# INLINE compare #-}
    compare lhs rhs =
        case comparing dim lhs rhs of
          EQ -> comparing nat lhs rhs
          v  -> v


-- |
-- /Since: 0.1.0.0/
instance Semigroup BitVector where

    {-# INLINE (<>) #-}
    (<>) (BV x m) (BV y n) = BV (x + y) $ (n `shiftL` fromEnum x) + m

    {-# INLINABLE sconcat #-}
    sconcat xs = BV w' n'
      where
        (w', _, n') = foldl' f (0, 0, 0) xs
        f (bitCountW, bitCountI, natVal) (BV w n) =
          (bitCountW + w, bitCountI + fromEnum w, natVal + (n `shiftL` bitCountI))

    {-# INLINE stimes #-}
    stimes 0  _       = mempty
    stimes e (BV w n) = BV limit $ go start n
      where
        !x     = fromEnum w
        !start = fromEnum $ limit - w
        !limit = (toEnum . fromEnum) e * w
        go  0 !acc = acc
        go !k !acc = go (k-x) $ (n `shiftL` k) + acc


-- |
-- /Since: 0.1.0.0/
instance Show BitVector where

    show (BV w n) = mconcat [ "[", show w, "]", show n ]


-- |
-- Create a bit vector from a /little-endian/ list of bits.
--
-- The following will hold:
--
-- > length . takeWhile not === countLeadingZeros . fromBits
-- > length . takeWhile not . reverse === countTrailingZeros . fromBits
--
-- /Time:/ \(\, \mathcal{O} \left( n \right) \)
--
-- /Since: 0.1.0.0/
--
-- ==== __Examples__
--
-- >>> fromBits [True, False, False]
-- [3]1
{-# INLINE fromBits #-}
fromBits :: Foldable f => f Bool -> BitVector
fromBits bs = BV (toEnum n) k
  -- NB: 'setBit' is a GMP function, faster than regular addition.
  where
    (!n, !k) = foldl' go (0, 0) bs
    go (!i, !v) b
      | b         = (i+1, v `setBit` i)
      | otherwise = (i+1, v)


-- |
-- Create a /little-endian/ list of bits from a bit vector.
--
-- The following will hold:
--
-- > length . takeWhile not . toBits === countLeadingZeros
-- > length . takeWhile not . reverse . toBits === countTrailingZeros
--
-- /Time:/ \(\, \mathcal{O} \left( n \right) \)
--
-- /Since: 0.1.0.0/
--
-- ==== __Examples__
--
-- >>> toBits [4]11
-- [True, True, False, True]
{-# INLINE toBits #-}
toBits :: BitVector -> [Bool]
toBits (BV w n) = go (fromEnum w) []
  where
    go 0 bs = bs
    go i bs = let !j = i - 1
              in go j $ n `testBit` j : bs


-- |
-- Create a bit vector of non-negative dimension from an integral value.
--
-- The integral value will be treated as an /signed/ number and the resulting
-- bit vector will contain the two's complement bit representation of the number.
--
-- The integral value will be interpreted as /little-endian/ so that the least
-- significant bit of the integral value will be the value of the 0th index of
-- the resulting bit vector and the most significant bit of the integral value
-- will be at index @dimension − 1@.
--
-- Note that if the bit representation of the integral value exceeds the
-- supplied dimension, then the most significant bits will be truncated in the
-- resulting bit vector.
--
-- /Time:/ \(\, \mathcal{O} \left( 1 \right) \)
--
-- /Since: 0.1.0.0/
--
-- ==== __Examples__
--
-- >>> fromNumber 8 96
-- [8]96
--
-- >>> fromNumber 8 -96
-- [8]160
--
-- >>> fromNumber 6 96
-- [6]32
{-# INLINE fromNumber #-}
fromNumber
  :: Integral v
  => Word  -- ^ dimension of bit vector
  -> v     -- ^ /signed, little-endian/ integral value
  -> BitVector
fromNumber !dimValue !intValue = BV dimValue . intToNat $ mask .&. v
  where
    !v | signum int < 0 = negate $ (shiftL 1 intBits) - int
       | otherwise      = int

    !int     = toInteger intValue
    !intBits = I# (integerLog2# int)
    !mask    = 2 ^ dimValue - 1


-- |
-- Two's complement value of a bit vector.
--
-- /Time:/ \(\, \mathcal{O} \left( 1 \right) \)
--
-- /Since: 0.1.0.0/
--
-- ==== __Examples__
--
-- >>> toSignedNumber [4]0
-- 0
--
-- >>> toSignedNumber [4]3
-- 3
--
-- >>> toSignedNumber [4]7
-- 7
--
-- >>> toSignedNumber [4]8
-- -8
--
-- >>> toSignedNumber [4]12
-- -4
--
-- >>> toSignedNumber [4]15
-- -1
{-# INLINE toSignedNumber #-}
toSignedNumber :: Num a => BitVector -> a
toSignedNumber (BV w n) = fromInteger v
  where
    !i = toInteger n
    !v | n `testBit` (fromEnum w - 1) = negate $ (shiftL 1 (fromEnum w)) - i
       | otherwise = i


-- |
-- Unsigned value of a bit vector.
--
-- /Time:/ \(\, \mathcal{O} \left( 1 \right) \)
--
-- /Since: 0.1.0.0/
--
-- ==== __Examples__
--
-- >>> toSignedNumber [4]0
-- 0
--
-- >>> toSignedNumber [4]3
-- 3
--
-- >>> toSignedNumber [4]7
-- 7
--
-- >>> toSignedNumber [4]8
-- 8
--
-- >>> toSignedNumber [4]12
-- 12
--
-- >>> toSignedNumber [4]15
-- 15
{-# INLINE toUnsignedNumber #-}
toUnsignedNumber :: Num a => BitVector -> a
toUnsignedNumber = fromInteger . toInteger . nat


-- |
-- Get the dimension of a 'BitVector'. Preferable to 'finiteBitSize' as it
-- returns a type which cannot represent a non-negative value and a 'BitVector'
-- must have a non-negative dimension.
--
-- /Time:/ \(\, \mathcal{O} \left( 1 \right) \)
--
-- /Since: 0.1.0.0/
--
-- ==== __Examples__
--
-- >>> dimension [2]3
-- 2
--
-- >>> dimension [4]12
-- 4
{-# INLINE dimension #-}
dimension :: BitVector -> Word
dimension = dim


-- |
-- Determine if /any/ bits are set in the 'BitVector'.
-- Faster than @(0 ==) . popCount@.
--
-- /Time:/ \(\, \mathcal{O} \left( 1 \right) \)
--
-- /Since: 0.1.0.0/
--
-- ==== __Examples__
--
-- >>> isZeroVector [2]3
-- False
--
-- >>> isZeroVector [4]0
-- True
{-# INLINE isZeroVector #-}
isZeroVector :: BitVector -> Bool
isZeroVector = (0 ==) . nat


-- |
-- Get the /inclusive/ range of bits in 'BitVector' as a new 'BitVector'.
--
-- If either of the bounds of the subrange exceed the bit vector's dimension,
-- the resulting subrange will append an infinite number of zeroes to the end
-- of the bit vector in order to satisfy the subrange request.
--
-- /Time:/ \(\, \mathcal{O} \left( 1 \right) \)
--
-- /Since: 0.1.0.0/
--
-- ==== __Examples__
--
-- >>> subRange (0,2) [4]7
-- [3]7
--
-- >>> subRange (1, 3) [4]7
-- [3]3
--
-- >>> subRange (2, 4) [4]7
-- [3]1
--
-- >>> subRange (3, 5) [4]7
-- [3]0
--
-- >>> subRange (10, 20) [4]7
-- [10]0
{-# INLINE subRange #-}
subRange :: (Word, Word) -> BitVector -> BitVector
subRange (!lower, !upper) (BV _ n)
  | lower > upper = zeroBits
  | otherwise     =
    case toInt lower of
      Nothing -> zeroBits
      Just i  ->
        let b = n `shiftR` i
        in  case toInt upper of
              Nothing ->
                let m = toEnum $ maxBound - i + 1
                in  BV m $  n `shiftR` i
              Just j  ->
                let x = j - i
                    m | x == maxBound = x
                      | otherwise     = x + 1
                in  BV (toEnum m) $ b .&. pred (1 `shiftL` m)


toInt :: Word -> Maybe Int
toInt w
  | w > maxInt = Nothing
  | otherwise  = Just $ fromEnum w
  where
    maxInt = toEnum (maxBound :: Int)



-- |
-- While similar to the function 'naturalFromInteger' exported from GHC.Natural,
-- this function does not throw an exception when an negative valued 'Integer'
-- is supplied and is also compatible with base < 4.10.0.0.
{-# INLINE intToNat #-}
intToNat :: Integer -> Natural
intToNat (S# i#) | I# i# >= 0  = NatS# (int2Word# i#)
intToNat (Jp# bn)              = NatJ# bn
intToNat _                     = NatS# (int2Word# 0#)


{-
{-# INLINE countLeadingOnes #-}
countLeadingOnes (BV w      0) = 0
countLeadingOnes (BV w natVal) = countLeadingZeroes $ compliment natVal
-}
