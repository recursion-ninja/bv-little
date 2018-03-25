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
import Test.QuickCheck (Arbitrary(..), CoArbitrary(..), NonNegative(..), suchThat)


-- |
-- A little-endian bit vector of non-negative dimension.
data  BitVector
    = BV
    { dim :: {-# UNPACK #-} !Int -- ^ The /dimension/ of a bit vector.
    , nat :: !Integer            -- ^ The value of a bit vector, as a natural number.
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
        dimVal <- toEnum . getNonNegative <$> arbitrary
        let upperBound = shiftL 1 dimVal
        intVal <- (getNonNegative <$> arbitrary) `suchThat` (< upperBound)
        pure $ BV dimVal intVal


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
    complement (BV w n) = BV w $ (shiftL 1 w) - 1 - n

    {-# INLINE zeroBits #-}
    zeroBits = BV 0 0

    {-# INLINE bit #-}
    bit i = BV (i+1) (shiftL 1 i)

    {-# INLINE clearBit #-}
    clearBit bv@(BV w n) i
      | i < 0 || i >= w = bv
      | otherwise       = BV w $ n `clearBit` i

{-
    {-# INLINE setBit #-}
    setBit bv@(BV w n) i
      | i < 0 || i >= w = bv
      | otherwise       = BV w $ n `setBit` i
-}

    {-# INLINE testBit #-}
    testBit (BV w n) i = i >= 0 && i < w && n `testBit` i

    bitSize (BV w _) = w

    {-# INLINE bitSizeMaybe #-}
    bitSizeMaybe (BV w _) = Just w

    {-# INLINE isSigned #-}
    isSigned = const False

    {-# INLINE shiftL #-}
    shiftL (BV w n) k
      | k > w     = BV w 0
      | otherwise = BV w $ shiftL n k .&. pred (shiftL 1 w)

    {-# INLINE shiftR #-}
    shiftR (BV w n) k
      | k > w     = BV w 0
      | otherwise = BV w $ shiftR n k

    {-# INLINE rotateL #-}
    rotateL bv       0 = bv
    rotateL bv@(BV w n) k
      | 0 == w    = bv
      | k == w    = BV w n
      | k >  w    = rotateL (BV w n) (k `mod` w)
      | otherwise = BV w $ h + l
      where
        s = w - k
        l = n `shiftR` s
        h = (n `shiftL` k) .&. pred (shiftL 1 w)

    {-# INLINE rotateR #-}
    rotateR bv       0 = bv
    rotateR bv@(BV w n) k
      | 0 == w    = bv
      | k == w    = bv
      | k >  w    = rotateR (BV w n) (k `mod` w)
      | otherwise = BV w $ h + l
      where
        s = w - k
        l = n `shiftR` k
        h = (n `shiftL` s) .&. pred (shiftL 1 w)

    {-# INLINE popCount #-}
    popCount = popCount . nat


-- |
-- /Since: 0.1.0.0/
instance CoArbitrary BitVector


-- |
-- /Since: 0.1.0.0/
instance Eq BitVector where

    {-# INLINE (==) #-}
    (==) (BV w1 m) (BV w2 n) = w1 == w2 && m == n


-- |
-- /Since: 0.1.0.0/
instance FiniteBits BitVector where

    {-# INLINE finiteBitSize #-}
    finiteBitSize = dim

    {-# INLINE countTrailingZeros #-}
    countTrailingZeros (BV w n) = max 0 $ w - lastSetBit - 1
      where
        lastSetBit = I# (integerLog2# n)

    {-# INLINE countLeadingZeros #-}
    countLeadingZeros (BV w      0) = w
    countLeadingZeros (BV w intVal) =
        case intVal of
          S#       v  -> countTrailingZeros $ iMask .|. I# v
          Jp# (BN# v) -> f $ ByteArray v
          Jn# (BN# v) -> f $ ByteArray v
      where
        iMask = complement zeroBits `xor` (2 ^ w - 1)

        f :: ByteArray -> Int
        f byteArr = g 0
          where
            (q, r) = w `quotRem` 64
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

    hash (BV w n) = w `hashWithSalt` hash n

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
    ofoldMap f = mconcat . fmap f. toBits

    {-# INLINE ofoldr #-}
    ofoldr f e = foldr f e . toBits

    {-# INLINE ofoldl' #-}
    ofoldl' f e = foldl' f e . toBits

    {-# INLINE ofoldr1Ex #-}
    ofoldr1Ex f = foldr1 f . toBits

    {-# INLINE ofoldl1Ex' #-}
    ofoldl1Ex' f = foldl1 f . toBits

    {-# INLINE onull #-}
    onull   = (== 0) . dim

    {-# INLINE olength #-}
    olength = dim


-- |
-- /Since: 0.1.0.0/
instance MonoFunctor BitVector where

    omap f (BV w n) = BV w . go w $ n `xor` n
    -- NB: 'setBit' is a GMP function, faster than regular addition.
      where
        go  0 !acc = acc
        go !i !acc = go i' acc'
          where
            i' = i - 1
            acc'
              | f (testBit n i') = acc `setBit` i'
              | otherwise        = acc



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
    (<>) (BV x m) (BV y n) = BV (x + y) $ (n `shiftL` x) + m

    {-# INLINABLE sconcat #-}
    sconcat xs = uncurry BV $ foldl' f (0,0) xs
      where
        f (bitCount, natVal) (BV w n) = (bitCount + w, natVal + (n `shiftL` bitCount))

    {-# INLINE stimes #-}
    stimes 0  _       = mempty
    stimes e (BV w n) = BV limit $ go (limit - w) n
      where
        limit = fromEnum e * w
        go  0 !acc = acc
        go !k !acc = go (k-w) $ (n `shiftL` k) + acc


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
fromBits bs = BV n k
  -- NB: 'setBit' is a GMP function, faster than regular addition.
  where
    (!n, !k) = foldl' go (0, 0) bs
    go (!i, !v) b
      | b         = (i+1, setBit v i)
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
toBits (BV w n) = testBit n <$> [ 0 .. w - 1 ]


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
fromNumber !dimValue !intValue = BV width $ mask .&. v
  where
    !v | signum int < 0 = negate $ (shiftL 1 intBits) - int
       | otherwise      = int

    !int     = toInteger intValue
    !intBits = I# (integerLog2# int)
    !width   = fromEnum dimValue
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
    v | n `testBit` (w-1) = negate $ (shiftL 1 w) - n
      | otherwise         = n


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
toUnsignedNumber = fromInteger . nat


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
dimension = toEnum . dim


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
                let m = maxBound - i + 1
                in  BV m b
              Just j  ->
                let x = j - i
                    m | x == maxBound = x
                      | otherwise     = x + 1
                in  BV m $ b .&. pred (1 `shiftL` m)


toInt :: Word -> Maybe Int
toInt w
  | w > maxInt = Nothing
  | otherwise  = Just $ fromEnum w
  where
    maxInt = toEnum (maxBound :: Int)
