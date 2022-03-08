{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Safe                       #-}

module Operator.Binary.Comparison
  ( ComparisonOperator(getComparator)
  ) where

import Control.DeepSeq
import Data.List                     (elemIndex)
import Data.Maybe                    (fromJust)
import Data.Monoid                   ()
import GHC.Generics
import Test.QuickCheck        hiding (generate)
import Test.SmallCheck.Series


newtype ComparisonOperator = CO { getComparator :: Bool -> Bool -> Ordering }
    deriving anyclass (NFData)
    deriving stock    (Generic)


comparatorList :: [ComparisonOperator]
comparatorList = do
    w <- [minBound .. maxBound]
    x <- [minBound .. maxBound]
    y <- [minBound .. maxBound]
    z <- [minBound .. maxBound]
    pure . CO $ \a b -> if not a && not b then w
                   else if not a &&     b then x
                   else if     a && not b then y
                   else {-     a &&     b   -} z


instance Arbitrary ComparisonOperator where

    arbitrary = arbitraryBoundedEnum


instance Bounded ComparisonOperator where

    minBound = head comparatorList

    maxBound = last comparatorList


instance CoArbitrary ComparisonOperator where

    coarbitrary = coarbitraryEnum


instance Enum ComparisonOperator where

    toEnum n = let !i = n `quot` length comparatorList
               in  comparatorList !! i

    fromEnum c = fromJust $ elemIndex c comparatorList


instance Eq ComparisonOperator where

    (CO f) == (CO g) = and
        [ f False False == g False False
        , f False True  == g False True
        , f True  False == g True  False
        , f True  True  == g True  True
        ]


instance Monad m => Serial m ComparisonOperator where

    series = generate $ const comparatorList


instance Show ComparisonOperator where

    show (CO f) = unlines
        [ ""
        , "f / F F -> "  <> show (f False False)
        , "  | F T -> "  <> show (f False True )
        , "  | T F -> "  <> show (f True  False)
        , "  \\ T T -> " <> show (f True  True )
        ]
