-----------------------------------------------------------------------------
-- |
-- Module      :  Data.BitVector.LittleEndian.Binary
-- Copyright   :  (c) Alex Washburn 2018
-- License     :  BSD-style
--
-- Maintainer  :  github@recursion.ninja
-- Stability   :  provisional
-- Portability :  portable
--
-- Exposes the 'Abitrary' and 'CoArbitrary' instances for 'BitVector'.
--
-----------------------------------------------------------------------------


{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# Language Safe #-}

module Data.BitVector.LittleEndian.QuickCheck
    (
    ) where

import Data.BitVector.LittleEndian.Internal
import Data.Bits
import Data.Monoid ()
import GHC.Natural
import Test.QuickCheck (Arbitrary(..), CoArbitrary(..), NonNegative(..), choose, suchThat, variant)


-- |
-- @since 0.1.0
instance Arbitrary BitVector where

    -- Arbitrary instance distribution weighting:
    --  -  2% = (maxBound :: Word)
    --  -  2% = (maxBound :: Word) + 1
    --  -  8% = all bits on
    --  -  8% = all bits off
    --  - 80% = any bit configuration
    arbitrary = do
        -- 1/25 chance of generating the boundary value at which the natural number
        -- must use different Natural constructors: NatS# & NatJ#
        n <- choose (0, 25 :: Word)
        case n of
            0 -> boundaryValue
            1 -> allBitsOn
            2 -> allBitsOn
            3 -> allBitsOff
            4 -> allBitsOff
            _ -> anyBitValue
        where
            allBitsOn     = genBitVector $ Just True
            allBitsOff    = genBitVector $ Just False
            anyBitValue   = genBitVector Nothing

            boundaryValue = do
                let wrdVal = maxBound :: Word
                let dimVal = toEnum $ popCount wrdVal
                let numVal = wordToNatural wrdVal
                -- 50/50 change to generate above or below the constructor boundary
                underBoundary <- arbitrary
                let (lowerBound, naturalVal)
                        | underBoundary = (dimVal, numVal)
                        | otherwise     = (dimVal + 1, numVal + 1)
                widthVal <- (getNonNegative <$> arbitrary) `suchThat` (>= lowerBound)
                pure $ BV widthVal naturalVal

            genBitVector spec = do
                dimVal <- getNonNegative <$> arbitrary
                let upperBound = shiftL 1 dimVal
                -- 1/5 chance all bits on or all bits off
                natVal <- case spec of
                    Just False -> pure $ naturalFromInteger 0
                    Just True  -> pure . naturalFromInteger $ upperBound - 1
                    Nothing ->
                        fmap naturalFromInteger $ (getNonNegative <$> arbitrary) `suchThat` (< upperBound)
                pure $ BV (toEnum dimVal) natVal


-- |
-- @since 0.1.0
instance CoArbitrary BitVector where

    coarbitrary bv = variant (dimension bv)
