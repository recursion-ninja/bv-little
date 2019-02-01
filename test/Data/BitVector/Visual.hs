{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.BitVector.Visual where

import Control.DeepSeq
import Data.Bits
import Data.BitVector.LittleEndian
import Data.Data
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Hashable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid ()
import Data.MonoTraversable
import Data.Semigroup
import GHC.Generics
import GHC.Natural
import Test.QuickCheck        hiding (generate)
import Test.SmallCheck.Series


newtype VisualBitVector = VBV BitVector
    deriving (Data, Eq, Ord, Generic, NFData, Typeable)


newtype VisualBitVectorSmall = VBVS BitVector
    deriving (Data, Eq, Ord, Generic, NFData, Typeable)


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
        go i = VBV $ fromNumber (toEnum dim) num
          where
            num = i - off
            off = (bit dim - 1)
            dim = logBase2 $ i + 1

        logBase2 x = finiteBitSize x - 1 - countLeadingZeros x

    fromEnum (VBV bv) =
        case dim of
          0 -> 0
          n -> num + 2^dim - 1
      where
        num = toUnsignedNumber bv
        dim = dimension bv


instance Enum VisualBitVectorSmall where

    toEnum n = go $ n `mod` (bit 4 - 1)
      where
        go i = VBVS $ fromNumber (toEnum dim) num
          where
            num = i - off
            off = (bit dim - 1)
            dim = logBase2 $ i + 1

        logBase2 x = finiteBitSize x - 1 - countLeadingZeros x

    fromEnum (VBVS bv) =
        case dim of
          0 -> 0
          n -> num + 2^dim - 1
      where
        num = toUnsignedNumber bv
        dim = dimension bv


instance Monad m => Serial m VisualBitVector where

    series = generate $ const allVBVs
      where
        allVBVs = toEnum <$> [0 .. fromEnum (maxBound :: VisualBitVector)]


instance Monad m => Serial m VisualBitVectorSmall where

    series = generate $ const allVBVs
      where
        allVBVs = toEnum <$> [0 .. fromEnum (maxBound :: VisualBitVectorSmall)]


instance Show VisualBitVector where

    show (VBV bv) = mconcat
      [ "["
      , show $ dimension bv
      , "]"
      , "<"
      , foldMap (\b -> if b then "1" else "0") $ toBits bv
      , ">"
      ]

instance Show VisualBitVectorSmall where

    show (VBVS bv) = mconcat
      [ "["
      , show $ dimension bv
      , "]"
      , "<"
      , foldMap (\b -> if b then "1" else "0") $ toBits bv
      , ">"
      ]
