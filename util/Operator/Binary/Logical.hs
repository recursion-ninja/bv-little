{-# Language DeriveAnyClass #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language Safe #-}

module Operator.Binary.Logical
    ( BinaryLogicalOperator ()
    , fromBinaryLogicalFunction
    , getBinaryLogicalOperator
    , getBinaryLogicalSymbol
    ) where

import Control.DeepSeq
import Data.Data
import Data.Monoid ()
import GHC.Generics
import Test.QuickCheck hiding (generate)
import Test.SmallCheck.Series


data BinaryLogicalOperator
    = AlwaysFalse
    | LogicalNOR
    | ConverseNonImplication
    | NotFirstArgument
    | NonImplication
    | NotSecondArgument
    | LogicalXOR
    | LogicalNAND
    | LogicalAND
    | LogicalXNOR
    | SecondArgument
    | Implication
    | FirstArgument
    | ConverseImplication
    | LogicalOR
    | AlwaysTrue
    deriving anyclass (NFData)
    deriving stock (Data, Eq, Generic, Ord)


instance Arbitrary BinaryLogicalOperator where

    arbitrary = arbitraryBoundedEnum


instance Bounded BinaryLogicalOperator where

    minBound = AlwaysFalse

    maxBound = AlwaysTrue


instance CoArbitrary BinaryLogicalOperator where

    coarbitrary = coarbitraryEnum


instance Enum BinaryLogicalOperator where

    toEnum n = case n `rem` 16 of
        0  -> AlwaysFalse
        1  -> LogicalNOR
        2  -> ConverseNonImplication
        3  -> NotFirstArgument
        4  -> NonImplication
        5  -> NotSecondArgument
        6  -> LogicalXOR
        7  -> LogicalNAND
        8  -> LogicalAND
        9  -> LogicalXNOR
        10 -> SecondArgument
        11 -> Implication
        12 -> FirstArgument
        13 -> ConverseImplication
        14 -> LogicalOR
        _  -> AlwaysTrue

    fromEnum x = case x of
        AlwaysFalse            -> 0
        LogicalNOR             -> 1
        ConverseNonImplication -> 2
        NotFirstArgument       -> 3
        NonImplication         -> 4
        NotSecondArgument      -> 5
        LogicalXOR             -> 6
        LogicalNAND            -> 7
        LogicalAND             -> 8
        LogicalXNOR            -> 9
        SecondArgument         -> 10
        Implication            -> 11
        FirstArgument          -> 12
        ConverseImplication    -> 13
        LogicalOR              -> 14
        AlwaysTrue             -> 15

    succ x = case x of
        AlwaysFalse            -> LogicalNOR
        LogicalNOR             -> ConverseNonImplication
        ConverseNonImplication -> NotFirstArgument
        NotFirstArgument       -> NonImplication
        NonImplication         -> NotSecondArgument
        NotSecondArgument      -> LogicalXOR
        LogicalXOR             -> LogicalNAND
        LogicalNAND            -> LogicalAND
        LogicalAND             -> LogicalXNOR
        LogicalXNOR            -> SecondArgument
        SecondArgument         -> Implication
        Implication            -> FirstArgument
        FirstArgument          -> ConverseImplication
        ConverseImplication    -> LogicalOR
        LogicalOR              -> AlwaysTrue
        AlwaysTrue             -> AlwaysFalse

    pred x = case x of
        AlwaysFalse            -> AlwaysTrue
        LogicalNOR             -> AlwaysFalse
        ConverseNonImplication -> LogicalNOR
        NotFirstArgument       -> ConverseNonImplication
        NonImplication         -> NotFirstArgument
        NotSecondArgument      -> NonImplication
        LogicalXOR             -> NotSecondArgument
        LogicalNAND            -> LogicalXOR
        LogicalAND             -> LogicalNAND
        LogicalXNOR            -> LogicalAND
        SecondArgument         -> LogicalXNOR
        Implication            -> SecondArgument
        FirstArgument          -> Implication
        ConverseImplication    -> FirstArgument
        LogicalOR              -> ConverseImplication
        AlwaysTrue             -> LogicalOR


--    enumFrom x = toEnum <$> [fromEnum x .. 15]


--    enumFromTo x y =


instance Monad m => Serial m BinaryLogicalOperator where

    series = generate $ const [minBound .. maxBound]


instance Show BinaryLogicalOperator where

    show x = "f p q = " <> s
        where
            s = case x of
                AlwaysFalse            -> "False (Contradiction)"
                LogicalNOR             -> "¬p ∧ ¬q (Logical NOR)"
                ConverseNonImplication -> "¬p ∧ q (Converse Non-Implication)"
                NotFirstArgument       -> "¬p (Not First)"
                NonImplication         -> "p ∧ ¬q (Non-Implication)"
                NotSecondArgument      -> "¬q (Not Second)"
                LogicalXOR             -> "(p ∧ ¬q) ∨ (¬p ∧ q) (Logical XOR)"
                LogicalNAND            -> "¬p ∨ ¬q (Logical NAND)"
                LogicalAND             -> "p ∧ q (Logical AND)"
                LogicalXNOR            -> "(p ∧ q) ∨ (¬p ∧ ¬q) (Logical XNOR)"
                SecondArgument         -> "q (Second)"
                Implication            -> "¬p ∨ q (Implication)"
                FirstArgument          -> "p (First)"
                ConverseImplication    -> "p ∨ ¬q (Converse Implication)"
                LogicalOR              -> "p ∧ q (Logical OR)"
                AlwaysTrue             -> "True (Tautology)"


getBinaryLogicalOperator :: BinaryLogicalOperator -> Bool -> Bool -> Bool
getBinaryLogicalOperator x = case x of
    AlwaysFalse            -> const (const False)
    LogicalNOR             -> \p q -> not $ p || q
    ConverseNonImplication -> \p q -> not p && q
    NotFirstArgument       -> \p _ -> not p
    NonImplication         -> \p q -> p && not q
    NotSecondArgument      -> \_ q -> not q
    LogicalXOR             -> (/=)
    LogicalNAND            -> \p q -> not $ p && q
    LogicalAND             -> (&&)
    LogicalXNOR            -> (==)
    SecondArgument         -> \_ q -> q
    Implication            -> \p q -> not p || q
    FirstArgument          -> const
    ConverseImplication    -> \p q -> p || not q
    LogicalOR              -> (||)
    AlwaysTrue             -> const (const True)


getBinaryLogicalSymbol :: BinaryLogicalOperator -> String
getBinaryLogicalSymbol x = case x of
    AlwaysFalse            -> "(const False)"
    LogicalNOR             -> "(not . (||))"
    ConverseNonImplication -> "(</=)"
    NotFirstArgument       -> "(not . fst)"
    NonImplication         -> "(=/>)"
    NotSecondArgument      -> "(not . snd)"
    LogicalXOR             -> "(/=)"
    LogicalNAND            -> "(not . (&&))"
    LogicalAND             -> "(&&)"
    LogicalXNOR            -> "(==)"
    SecondArgument         -> "(snd)"
    Implication            -> "(==>)"
    FirstArgument          -> "(fst)"
    ConverseImplication    -> "(<==)"
    LogicalOR              -> "(||)"
    AlwaysTrue             -> "(const True)"


fromBinaryLogicalFunction :: (Bool -> Bool -> Bool) -> BinaryLogicalOperator
fromBinaryLogicalFunction f = case (f True True, f True False, f False True, f False False) of
    (False, False, False, False) -> AlwaysFalse
    (False, False, False, True ) -> LogicalNOR
    (False, False, True , False) -> ConverseNonImplication
    (False, False, True , True ) -> NotFirstArgument
    (False, True , False, False) -> NonImplication
    (False, True , False, True ) -> NotSecondArgument
    (False, True , True , False) -> LogicalXOR
    (False, True , True , True ) -> LogicalNAND
    (True , False, False, False) -> LogicalAND
    (True , False, False, True ) -> LogicalXNOR
    (True , False, True , False) -> SecondArgument
    (True , False, True , True ) -> Implication
    (True , True , False, False) -> FirstArgument
    (True , True , False, True ) -> ConverseImplication
    (True , True , True , False) -> LogicalOR
    (True , True , True , True ) -> AlwaysTrue
