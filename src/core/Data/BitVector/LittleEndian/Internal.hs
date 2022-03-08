-----------------------------------------------------------------------------
-- |
-- Module      :  Data.BitVector.LittleEndian.Internal
-- Copyright   :  (c) Alex Washburn 2020
-- License     :  BSD-style
--
-- Maintainer  :  github@recursion.ninja
-- Stability   :  provisional
-- Portability :  portable
--
-- Little-endian bit vectors are isomorphic to a @[Bool]@ with the /least/
-- significant bit at the head of the list and the /most/ significant bit at the
-- end of the list. Consequently, the endianness of a bit vector affects the semantics of the
-- following typeclasses:
--
--   * Bits
--   * FiniteBits
--   * Semigroup
--   * Monoid
--   * MonoAdjustable
--   * MonoIndexable
--   * MonoKeyed
--   * MonoLookup
--   * MonoFoldable
--   * MonoFoldableWithKey
--   * MonoTraversable
--   * MonoTraversableWithKey
--   * MonoZipWithKey
--
-----------------------------------------------------------------------------

{-# Language BangPatterns #-}
{-# Language CPP #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language MagicHash #-}
{-# Language OverloadedStrings #-}
{-# Language Trustworthy #-}
{-# Language TypeFamilies #-}
{-# Language UnicodeSyntax #-}

module Data.BitVector.LittleEndian.Internal
    ( BitVector(..)
    , fromBits
    , fromNumber
    , toBits
    , toSignedNumber
    , toUnsignedNumber
    , dimension
    , isZeroVector
    , subRange
    , rank
    , select
--    , bitsInWord
--    , wordRank
--    , wordSelect
    ) where

import Control.DeepSeq
import Control.Monad (when)
import Data.Bits
import Data.Data
import Data.Foldable
import Data.Hashable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid ()
import Data.Ord
import Data.Primitive.ByteArray
import Data.Semigroup
import GHC.Exts
import GHC.Generics (Generic)
import GHC.Integer.GMP.Internals
import GHC.Integer.Logarithms
import GHC.Natural
import Text.Read


-- |
-- A little-endian bit vector of non-negative dimension.
data BitVector 
    = BV
    { dim :: {-# UNPACK #-} !Word
      -- ^ The /dimension/ of a bit vector.
    , nat :: !Natural
      -- ^ The value of a bit vector, as a natural number.
    }
    -- ^ @since 0.1.0
    -- ^ @since 0.1.0
    -- ^ @since 0.1.0
    deriving stock (Data, Generic)


-- |
-- @since 0.1.0
instance Bits BitVector where

    {-# INLINE (.&.) #-}
    (BV w1 a) .&. (BV w2 b) = BV (max w1 w2) $ a .&. b

    {-# INLINE (.|.) #-}
    (BV w1 a) .|. (BV w2 b) = BV (max w1 w2) $ a .|. b

    {-# INLINE xor #-}
    (BV w1 a) `xor` (BV w2 b) = BV (max w1 w2) $ a `xor` b

    {-# INLINE complement #-}
    complement (BV w n) = BV w $ shiftL 1 (fromEnum w) - 1 - n

    {-# INLINE zeroBits #-}
    zeroBits = BV 0 0

    {-# INLINE bit #-}
    bit i = BV (succ $ toEnum i) (shiftL 1 i)

    {-# INLINE clearBit #-}
    -- We do this more complicated operation rather than call 'clearBit'
    -- because it is undefined for Natural in base < 4.10.0.0
    clearBit bv@(BV w n) i
        | i < 0 || toEnum i >= w
        = bv
        | otherwise
        = let
              !allBits = pred . shiftL 1 $ fromEnum w
              !mask    = bit i `xor` allBits
          in  BV w $ n .&. mask

    {-# INLINE setBit #-}
    setBit bv@(BV w n) i
        | i < 0     = bv
        | otherwise = BV (max w j) $ n `setBit` i
        where !j = toEnum i + 1

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
        | k < 0     = bv
        | j >= w    = go . fromEnum $ j `mod` w
        | otherwise = go k
        where
            !j = toEnum k
            go 0 = bv
            go i = BV w $ h + l
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
        | k < 0     = bv
        | j >= w    = go . fromEnum $ j `mod` w
        | otherwise = go k
        where
            !j = toEnum k
            go 0 = bv
            go i = BV w $ h + l
                where
                    !v = fromEnum w
                    !d = v - i
                    !m = pred $ shiftL 1 i
                    !l = n `shiftR` i
                    !h = (n .&. m) `shiftL` d

    {-# INLINE popCount #-}
    popCount = popCount . nat


-- |
-- @since 0.1.0
instance Eq BitVector where

    {-# INLINE (==) #-}
    (==) (BV w1 m) (BV w2 n) = w1 == w2 && (naturalToBigNat m) `compare` (naturalToBigNat n) == EQ
        where
            naturalToBigNat (NatS# w ) = wordToBigNat w
            naturalToBigNat (NatJ# bn) = bn


-- |
-- @since 0.1.0
instance FiniteBits BitVector where

    {-# INLINE finiteBitSize #-}
    finiteBitSize = fromEnum . dim

    {-# INLINE countTrailingZeros #-}
    countTrailingZeros (BV w n) = max 0 $ fromEnum w - lastSetBit - 1
        where lastSetBit = I# (integerLog2# (toInteger n))

    {-# INLINE countLeadingZeros #-}
    countLeadingZeros (BV w 0     ) = fromEnum w
    countLeadingZeros (BV w natVal) = case natVal of
        NatS# v       -> countTrailingZeros $ iMask .|. W# v
        NatJ# (BN# v) -> f $ ByteArray v
        where
            iMask = complement zeroBits `xor` (2 ^ w - 1)
            !x    = fromEnum w

            f :: ByteArray -> Int
            f byteArr = g 0
                where
                    (q, r) = x `quotRem` fromEnum bitsInWord
                    wMask  = complement zeroBits `xor` (2 ^ r - 1) :: Word

                    g :: Int -> Int
                    g !i
                        | i >= q    = countTrailingZeros $ wMask .|. value
                        | otherwise = let !v = countTrailingZeros value
                                      in  if v == fromEnum bitsInWord then v + g (i + 1) else v
                        where
                            value :: Word
                            value = byteArr `indexByteArray` i


-- |
-- @since 0.1.0
instance Hashable BitVector where

    hash (BV w n) = fromEnum w `hashWithSalt` hash n

    hashWithSalt salt bv = salt `hashWithSalt` hash bv


-- |
-- @since 0.1.0
instance Monoid BitVector where

    {-# INLINE mappend #-}
    mappend = (<>)

    {-# INLINE mconcat #-}
    mconcat bs = case bs of
        []     -> mempty
        x : xs -> sconcat $ x :| xs

    {-# INLINE mempty #-}
    mempty = BV 0 0


-- |
-- @since 0.1.0
instance NFData BitVector where

    -- Already a strict data type,
    -- always in normal form.
    {-# INLINE rnf #-}
    rnf = const ()


-- |
-- @since 0.1.0
instance Ord BitVector where

    {-# INLINE compare #-}
    compare lhs rhs = case comparing dim lhs rhs of
        EQ -> comparing nat lhs rhs
        v  -> v


-- |
-- @since 1.2.0
instance Read BitVector where

    {-# INLINABLE readPrec #-}
    readPrec = do
        Punc "[" <- lexP
        w        <- step readPrec
        Punc "]" <- lexP
        n        <- step readPrec
        -- Check if n exceeds the bit vector's width
        when (n >= 1 `shiftL` fromEnum w) pfail
        pure $ BV w n

    readListPrec = readListPrecDefault


-- |
-- @since 0.1.0
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
    stimes 0 _        = mempty
    stimes e (BV w n) = BV limit $ go start n
        where
            !x     = fromEnum w
            !start = fromEnum $ limit - w
            !limit = (toEnum . fromEnum) e * w
            go 0 !acc = acc
            go k !acc = go (k - x) $ (n `shiftL` k) + acc


-- |
-- @since 0.1.0
instance Show BitVector where

    show (BV w n) = mconcat ["[", show w, "]", show n]


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
-- /Since: 0.1.0/
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
        go :: Bits b => (Int, b) -> Bool -> (Int, b)
        go (!i, !v) b
            | b         = (i + 1, v `setBit` i)
            | otherwise = (i + 1, v)


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
-- /Since:/ 0.1.0
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
        go i bs = let !j = i - 1 in go j $ n `testBit` j : bs


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
-- /Since: 0.1.0/
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
{-# INLINE [1] fromNumber #-}
fromNumber
    :: Integral v
    => Word  -- ^ dimension of bit vector
    -> v     -- ^ /signed, little-endian/ integral value
    -> BitVector
fromNumber !dimValue !intValue = BV dimValue . intToNat $ mask .&. v
    where
        !v
            | signum int < 0 = negate $ shiftL 1 intBits - int
            | otherwise      = int

        !int     = toInteger intValue
        !intBits = I# (integerLog2# int)
        !mask    = 2 ^ dimValue - 1


{-# RULES
"fromNumber/Natural" forall w (n :: Natural).  fromNumber w n = BV w n
"fromNumber/Word"    forall w (v :: Word   ).  fromNumber w v = BV w (wordToNatural v)
  #-}


-- |
-- Two's complement value of a bit vector.
--
-- /Time:/ \(\, \mathcal{O} \left( 1 \right) \)
--
-- /Since: 0.1.0/
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
        !v
            | n `testBit` (fromEnum w - 1) = negate $ shiftL 1 (fromEnum w) - i
            | otherwise = i


-- |
-- Unsigned value of a bit vector.
--
-- /Time:/ \(\, \mathcal{O} \left( 1 \right) \)
--
-- /Since: 0.1.0/
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
{-# INLINE [1] toUnsignedNumber #-}
toUnsignedNumber :: Num a => BitVector -> a
toUnsignedNumber = fromInteger . toInteger . nat


{-# RULES
"toUnsignedNumber/Natural" toUnsignedNumber = nat
  #-}


-- |
-- Get the dimension of a 'BitVector'. Preferable to 'finiteBitSize' as it
-- returns a type which cannot represent a non-negative value and a 'BitVector'
-- must have a non-negative dimension.
--
-- /Time:/ \(\, \mathcal{O} \left( 1 \right) \)
--
-- /Since: 0.1.0/
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
-- /Since: 0.1.0/
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
-- /Since: 0.1.0/
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
    | otherwise = case toInt lower of
        Nothing -> zeroBits
        Just i ->
            let b = n `shiftR` i
            in
                case toInt upper of
                    Nothing -> let m = toEnum $ maxBound - i + 1 in BV m $ n `shiftR` i
                    Just j ->
                        let x = j - i
                            m
                                | x == maxBound = x
                                | otherwise     = x + 1
                        in  BV (toEnum m) $ b .&. pred (1 `shiftL` m)


-- |
-- Determine the number of /set/ bits in the 'BitVector' up to, /but not including/, index @k@.
--
-- To determine the number of /unset/ bits in the 'BitVector`, use @k - rank bv k@.
--
-- Uses "broadword programming." Efficient on small 'BitVector's (10^3).
--
-- /Time:/ \(\, \mathcal{O} \left( \frac{n}{w} \right) \), where \(w\) is the number of bits in a 'Word'.
--
-- /Since: 1.1.0/
--
-- ==== __Examples__
--
-- >>> let bv = fromNumber 128 0 `setBit` 0 `setBit` 65
--
-- >>> rank bv   0  -- Count how many ones in the first 0 bits (always returns 0)
-- 0
--
-- >>> rank bv   1  -- Count how many ones in the first 1 bits
-- 1
--
-- >>> rank bv   2  -- Count how many ones in the first 2 bits
-- 1
--
-- >>> rank bv  65  -- Count how many ones in the first 65 bits
-- 1
--
-- >>> rank bv  66  -- Count how many ones in the first 66 bits
-- 1
--
-- >>> rank bv 128  -- Count how many ones in all 128 bits
-- 2
--
-- >>> rank bv 129  -- Out-of-bounds, fails gracefully
-- 2
rank
    :: BitVector
    -> Word -- ^ \(k\), the rank index
    -> Word -- ^ Set bits within the rank index
rank _        0 = 0 -- There can be no set bits /before/ the 0-th bit
rank (BV 0 _) _ = 0 -- There can be no set bits in a bit-vector of length 0
rank (BV w natVal) k =
    let j = min k w
    in
        case natVal of
            NatS# v       -> wordRank (W# v) j
            NatJ# (BN# v) -> f (ByteArray v) j
    where
        f :: ByteArray -> Word -> Word
        f byteArr x = g x 0
            where
                g :: Word -> Int -> Word
                g !j !i
                    | j < bitsInWord = wordRank value j
                    | otherwise      = let !v = toEnum $ popCount value in v + g (j - bitsInWord) (i + 1)
                    where
                        value :: Word
                        value = byteArr `indexByteArray` i


-- |
-- Find the index of the k-th set bit in the 'BitVector'.
--
-- To find the index of the k-th /unset/ bit in the 'BitVector`, use @select (complement bv) k@.
--
-- Uses "broadword programming." Efficient on small 'BitVector's (10^3).
--
-- /Time:/ \(\, \mathcal{O} \left( \frac{n}{w} \right) \), where \(w\) is the number of bits in a 'Word'.
--
-- /Since: 1.1.0/
--
-- ==== __Examples__
--
-- >>> let bv = fromNumber 128 0 `setBit` 0 `setBit` 65
--
-- >>> select bv 0  -- Find the 0-indexed position of the first one bit
-- Just 0
--
-- >>> select bv 1  -- Find the 0-indexed position of the second one bit
-- Just 65
--
-- >>> select bv 2  -- There is no 3rd set bit, `select` fails
-- Nothing
select
    :: BitVector
    -> Word        -- ^ \(k\), the select index
    -> Maybe Word  -- ^ index of the k-th set bit
select (BV 0 _     ) _ = Nothing -- There can be no set bits in a bit-vector of length 0
select (BV w natVal) k = case natVal of
    NatS# v       -> let !u = W# v in if toEnum (popCount u) <= k then Nothing else Just $ wordSelect u k
    NatJ# (BN# v) -> f (ByteArray v) k
    where
        f :: ByteArray -> Word -> Maybe Word
        f byteArr x = g x 0
            where
                g :: Word -> Int -> Maybe Word
                g !j !i
                    | toEnum i * bitsInWord >= w = Nothing
                    | j < ones  = Just $ wordSelect value j
                    | otherwise = (bitsInWord +) <$> g (j - ones) (i + 1)
                    where
                        ones = toEnum $ popCount value
                        value :: Word
                        value = byteArr `indexByteArray` i


-- |
-- Number of bits in a 'Word'.
--
-- Used for "broadword programming."
{-# INLINE bitsInWord #-}
bitsInWord :: Word
bitsInWord = toEnum $ finiteBitSize (undefined :: Word)


-- |
-- Clever use of 'popCount' and masking to get the number of set bits up to,
-- /but not including/,  index "k."
wordRank
    :: Word -- ^ Input 'Word'
    -> Word -- ^ Index k, upt to which we count all set bits, k in range [ 0, finiteBitCount - 1 ]
    -> Word -- ^ THe number of bits set within index "k."
wordRank v x = toEnum . popCount $ suffixOnes .&. v where suffixOnes = (1 `shiftL` fromEnum x) - 1


-- |
-- Perform binary search with 'popCount' to locate the k-th set bit
wordSelect
    :: Word -- ^ Input 'Word'
    -> Word -- ^ Find the k-th set bit, k in range [ 0, finiteBitCount - 1 ]
    -> Word -- ^ The index of the k-th set bit
wordSelect v = go 0 63
    where
        go :: Word -> Word -> Word -> Word
        go lb ub x
            | lb + 1 == ub
            = if x == 0 && v `testBit` fromEnum lb then lb else ub
            | otherwise
            = let !lowOnes = toEnum . popCount $ lowMask .&. v
              in  if lowOnes > x then go lb mb x else go (mb + 1) ub (x - lowOnes)
            where
                mb      = ((ub - lb) `div` 2) + lb
                lowMask = makeMask lb mb

                makeMask :: Enum a => a -> Word -> Word
                makeMask i j = wideMask `xor` thinMask
                    where
                        thinMask = (1 `shiftL` fromEnum i) - 1
                        wideMask
                            | j == bitsInWord - 1 = maxBound :: Word
                            | otherwise           = (1 `shiftL` (fromEnum j + 1)) - 1


-- |
-- Convert a 'Word' to an 'Int', but only if the 'Word' value is small enough.
toInt :: Word -> Maybe Int
toInt w
    | w > maxInt = Nothing
    | otherwise  = Just $ fromEnum w
    where maxInt = toEnum (maxBound :: Int)


-- |
-- While similar to the function 'naturalFromInteger' exported from GHC.Natural,
-- this function does not throw an exception when an negative valued 'Integer'
-- is supplied and is also compatible with base < 4.10.0.0.
{-# INLINE intToNat #-}
-- {-# NOINLINE intToNat #-}
intToNat :: Integer -> Natural
intToNat = naturalFromInteger
