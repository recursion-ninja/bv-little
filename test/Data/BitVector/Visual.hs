{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language MultiParamTypeClasses #-}

module Data.BitVector.Visual
    ( HasBitVector (..)
    , VisualBitVector ()
    , VisualBitVectorSmall ()
    ) where

import Control.DeepSeq
import Data.BitVector.LittleEndian
import Data.Bits
import Data.Data
import Data.Foldable (fold)
import Data.Monoid ()
import GHC.Generics
import GHC.Natural
import Test.QuickCheck hiding (generate)
import Test.SmallCheck.Series


newtype VisualBitVector
    = VBV BitVector
    deriving newtype (Eq, NFData, Ord)
    deriving stock (Data, Generic)


newtype VisualBitVectorSmall
    = VBVS BitVector
    deriving newtype (Eq, NFData, Ord)
    deriving stock (Data, Generic)


class HasBitVector a where

    getBitVector :: a -> BitVector


instance HasBitVector BitVector where

    getBitVector = id


instance HasBitVector VisualBitVector where

    getBitVector (VBV bv) = bv


instance HasBitVector VisualBitVectorSmall where

    getBitVector (VBVS bv) = bv


instance CoArbitrary VisualBitVector where

    coarbitrary = coarbitraryEnum


instance CoArbitrary VisualBitVectorSmall where

    coarbitrary = coarbitraryEnum


{-
instance Monad m => CoSerial m VisualBitVector where

    coseries x = genericCoseries x
-}


instance Bounded VisualBitVector where

    minBound = VBV $ fromNumber 0 0

    maxBound = VBV $ fromNumber 8 (bit 8 - 1 :: Natural)


instance Bounded VisualBitVectorSmall where

    minBound = VBVS $ fromNumber 0 0

    maxBound = VBVS $ fromNumber 3 (bit 3 - 1 :: Natural)


instance Enum VisualBitVector where

    toEnum n = go $ n `mod` (bit 9 - 1)
        where
            go :: (Integral v, FiniteBits v) => v -> VisualBitVector
            go i = VBV $ fromNumber (toEnum dim) num where (num, _, dim) = getEnumContext i

    fromEnum (VBV bv) = case dim of
        0 -> 0
        n -> num + 2 ^ n - 1
        where
            num = toUnsignedNumber bv
            dim = dimension bv


instance Enum VisualBitVectorSmall where

    toEnum n = go $ n `mod` (bit 4 - 1)
        where
            go :: (Integral v, FiniteBits v) => v -> VisualBitVectorSmall
            go i = VBVS $ fromNumber (toEnum dim) num where (num, _, dim) = getEnumContext i

    fromEnum (VBVS bv) = case dim of
        0 -> 0
        n -> num + 2 ^ n - 1
        where
            num = toUnsignedNumber bv
            dim = dimension bv


instance Monad m => Serial m VisualBitVector where

    series = generate $ const allVBVs
        where allVBVs = toEnum <$> [0 .. fromEnum (maxBound :: VisualBitVector)]


instance Monad m => Serial m VisualBitVectorSmall where

    series = generate $ const allVBVs
        where allVBVs = toEnum <$> [0 .. fromEnum (maxBound :: VisualBitVectorSmall)]


instance Show VisualBitVector where

    show (VBV bv) =
        fold ["[", show $ dimension bv, "]", "<", foldMap (\b -> if b then "1" else "0") $ toBits bv, ">"]


instance Show VisualBitVectorSmall where

    show (VBVS bv) =
        fold ["[", show $ dimension bv, "]", "<", foldMap (\b -> if b then "1" else "0") $ toBits bv, ">"]


getEnumContext :: (FiniteBits b, Num b) => b -> (b, b, Int)
getEnumContext i = (num, off, dim)
    where
        num = i - off
        off = bit dim - 1
        dim = logBase2 $ i + 1
        logBase2 :: FiniteBits b => b -> Int
        logBase2 x = finiteBitSize x - 1 - countLeadingZeros x

