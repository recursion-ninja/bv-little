{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Operator.Unary.Logical
  ( UnaryLogicalOperator()
  , getUnaryLogicalSymbol
  , getUnaryLogicalOperator
  , fromUnaryLogicalFunction
  ) where

import Control.DeepSeq
import Data.Bits
import Data.BitVector.LittleEndian
import Data.Data
import Data.Hashable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid ()
import Data.MonoTraversable
import Data.Semigroup
import GHC.Generics
import Test.QuickCheck        hiding (generate)
import Test.SmallCheck.Series


data  UnaryLogicalOperator
    = AlwaysFalse
    | Identity
    | Negation
    | AlwaysTrue
    deriving (Data, Eq, Ord, Generic, NFData, Typeable)


instance Arbitrary UnaryLogicalOperator where

    arbitrary = arbitraryBoundedEnum


instance Bounded UnaryLogicalOperator where

    minBound = AlwaysFalse

    maxBound = AlwaysTrue


instance CoArbitrary UnaryLogicalOperator where

    coarbitrary = coarbitraryEnum


instance Enum UnaryLogicalOperator where

    toEnum n =
        case n `rem` 4 of
          0  -> AlwaysFalse
          1  -> Identity
          2  -> Negation
          3  -> AlwaysTrue

    fromEnum x =
        case x of
          AlwaysFalse -> 0
          Identity    -> 1
          Negation    -> 2
          AlwaysTrue  -> 3

    succ x =
        case x of
          AlwaysFalse -> Identity
          Identity    -> Negation
          Negation    -> AlwaysTrue
          AlwaysTrue  -> AlwaysFalse


    pred x =
        case x of
          AlwaysFalse -> AlwaysTrue
          Identity    -> AlwaysFalse
          Negation    -> Identity
          AlwaysTrue  -> Negation


instance Monad m => Serial m UnaryLogicalOperator where

    series = generate $ const [minBound .. maxBound]


instance Show UnaryLogicalOperator where

    show x = "f p q = " <> s
      where
        s = case x of
              AlwaysFalse -> "False (Contradiction)"
              Identity    -> "id (Identity)"
              Negation    -> "not (Negation)"
              AlwaysTrue  -> "True (Tautology)"


getUnaryLogicalOperator :: UnaryLogicalOperator -> Bool -> Bool
getUnaryLogicalOperator x =
    case x of
      AlwaysFalse -> const False
      Identity    -> id
      Negation    -> not
      AlwaysTrue  -> const True


getUnaryLogicalSymbol :: UnaryLogicalOperator -> String
getUnaryLogicalSymbol x =
    case x of
      AlwaysFalse -> "(const False)"
      Identity    -> "(id)"
      Negation    -> "(not)"
      AlwaysTrue  -> "(const True)"


fromUnaryLogicalFunction :: (Bool -> Bool) -> UnaryLogicalOperator
fromUnaryLogicalFunction f = 
    case (f False, f True) of
      (False, False) -> AlwaysFalse
      (False, True ) -> Identity
      (True , False) -> Negation
      (True , True ) -> AlwaysTrue

