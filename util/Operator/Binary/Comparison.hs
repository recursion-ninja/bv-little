{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Operator.Binary.Comparison
  ( ComparisonOperator(getComparator)
  ) where

import Control.DeepSeq
import Data.Bits
import Data.BitVector.LittleEndian
import Data.Data
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Hashable
import Data.List                     (elemIndex)
import Data.List.NonEmpty            (NonEmpty(..))
import Data.Maybe                    (fromJust)
import Data.Monoid                   ()
import Data.MonoTraversable
import Data.Semigroup
import GHC.Generics
import Test.QuickCheck        hiding (generate)
import Test.SmallCheck.Series


data ComparisonOperator = CO { getComparator :: Bool -> Bool -> Ordering }
    deriving (Generic, NFData, Typeable)


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
