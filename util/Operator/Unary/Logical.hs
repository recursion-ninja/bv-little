{-|

Copyright   : © 2020 Alex Washburn
License     : BSD-3-Clause
Maintainer  : github@recursion.ninja
Stability   : Stable

-}

{-# Language DeriveAnyClass #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language Safe #-}

module Operator.Unary.Logical
    ( UnaryLogicalOperator ()
    , fromUnaryLogicalFunction
    , getUnaryLogicalOperator
    , getUnaryLogicalSymbol
    ) where

import Control.DeepSeq
import Data.Data
import Data.Monoid ()
import GHC.Generics
import Test.QuickCheck hiding (generate)
import Test.SmallCheck.Series


{-|
Representation of all possible /unary/ operators of type @(Bool -> Bool)@.
Useful for both property and enumeration based testing.
-}
data UnaryLogicalOperator
    = AlwaysFalse
    | Identity
    | Negation
    | AlwaysTrue
    deriving anyclass (NFData)
    deriving stock (Data, Eq, Generic, Ord)


instance Arbitrary UnaryLogicalOperator where

    arbitrary = arbitraryBoundedEnum


instance Bounded UnaryLogicalOperator where

    minBound = AlwaysFalse

    maxBound = AlwaysTrue


instance CoArbitrary UnaryLogicalOperator where

    coarbitrary = coarbitraryEnum


instance Enum UnaryLogicalOperator where

    toEnum n = case n `rem` 4 of
        0 -> AlwaysFalse
        1 -> Identity
        2 -> Negation
        _ -> AlwaysTrue

    fromEnum x = case x of
        AlwaysFalse -> 0
        Identity    -> 1
        Negation    -> 2
        AlwaysTrue  -> 3

    succ x = case x of
        AlwaysFalse -> Identity
        Identity    -> Negation
        Negation    -> AlwaysTrue
        AlwaysTrue  -> AlwaysFalse

    pred x = case x of
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


{-|
Convert from a closed, unnary function over 'Bool' to a 'UnaryLogicalOperator'.
-}
fromUnaryLogicalFunction :: (Bool -> Bool) -> UnaryLogicalOperator
fromUnaryLogicalFunction f = case (f False, f True) of
    (False, False) -> AlwaysFalse
    (False, True ) -> Identity
    (True , False) -> Negation
    (True , True ) -> AlwaysTrue


{-|
Convert from a 'UnaryLogicalOperator' to a closed, unary function over 'Bool'.
-}
getUnaryLogicalOperator :: UnaryLogicalOperator -> Bool -> Bool
getUnaryLogicalOperator x = case x of
    AlwaysFalse -> const False
    Identity    -> id
    Negation    -> not
    AlwaysTrue  -> const True


{-|
Query the Haskell expression of a 'UnaryLogicalOperator' representation symbolically as a 'String'.
-}
getUnaryLogicalSymbol :: UnaryLogicalOperator -> String
getUnaryLogicalSymbol x = case x of
    AlwaysFalse -> "(const False)"
    Identity    -> "(id)"
    Negation    -> "(not)"
    AlwaysTrue  -> "(const True)"
