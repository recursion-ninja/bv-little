{-|

Copyright   : © 2020 Alex Washburn
License     : BSD-3-Clause
Maintainer  : github@recursion.ninja
Stability   : Stable

-}

{-# Language BangPatterns #-}
{-# Language DeriveAnyClass #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language Safe #-}

module Operator.Binary.Comparison
    ( ComparisonOperator (getComparator)
    ) where

import Control.DeepSeq
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Monoid ()
import GHC.Generics
import Test.QuickCheck hiding (generate)
import Test.SmallCheck.Series


{-|
Representation of all possible binary operators of type @(Bool -> Bool -> Bool)@.
Useful for both property and enumeration based testing.
-}
newtype ComparisonOperator
    = CO { getComparator :: Bool -> Bool -> Ordering }
    deriving anyclass (NFData)
    deriving stock (Generic)


comparatorList :: [ComparisonOperator]
comparatorList = do
    w <- [minBound .. maxBound]
    x <- [minBound .. maxBound]
    y <- [minBound .. maxBound]
    z <- [minBound .. maxBound]
    pure
        $ let
            op False False = w
            op False True  = x
            op True  False = y
            op True  True  = z
          in  CO op


instance Arbitrary ComparisonOperator where

    arbitrary = arbitraryBoundedEnum


instance Bounded ComparisonOperator where

    minBound = head comparatorList

    maxBound = last comparatorList


instance CoArbitrary ComparisonOperator where

    coarbitrary = coarbitraryEnum


instance Enum ComparisonOperator where

    toEnum n = let !i = n `quot` length comparatorList in comparatorList !! i

    fromEnum c = fromJust $ elemIndex c comparatorList


instance Eq ComparisonOperator where

    (CO f) == (CO g) = and
        [ f False False == g False False
        , f False True == g False True
        , f True False == g True False
        , f True True == g True True
        ]


instance Monad m => Serial m ComparisonOperator where

    series = generate $ const comparatorList


instance Show ComparisonOperator where

    show (CO f) = unlines
        [ ""
        , "f / F F -> " <> show (f False False)
        , "  | F T -> " <> show (f False True)
        , "  | T F -> " <> show (f True False)
        , "  \\ T T -> " <> show (f True True)
        ]
