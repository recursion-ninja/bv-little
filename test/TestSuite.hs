{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- We apply this to suppress the deprecated warning cause by calls to 'bitSize'
-- If there is a more fine-grained way to supress this warning without suppressing
-- deprecated warnings for the whole module, we should do that instead.
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Main ( main ) where

import Control.DeepSeq
import Data.Bits
import Data.BitVector.LittleEndian
import Data.BitVector.LittleEndian.Instances ()
import Data.BitVector.Visual
import Data.Foldable
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Hashable
import Data.Maybe
import Data.Monoid ()
import Data.MonoTraversable
import Data.MonoTraversable.Keys
import Data.Semigroup
import Operator.Binary.Comparison
import Operator.Binary.Logical
import Operator.Unary.Logical
import GHC.Exts (IsList(..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding ((.&.), forAll, testProperty)
import Test.Tasty.QuickCheck qualified as QC
import Test.Tasty.SmallCheck hiding ((==>), Property, testProperty)
import Test.Tasty.SmallCheck qualified as SC
--import TextShow (TextShow(showb), toString)


infix 0 -=>
(-=>) :: QC.Testable p => Bool -> p -> Property 
(-=>) p q = not p .||. q


main :: IO ()
main = defaultMain testSuite


testSuite :: TestTree
testSuite = testGroup "BitVector tests"
    [ bitsTests
    , finiteBitsTests
    , hashableTests
    , monoAdjustableProperties
    , monoFunctorProperties
    , monoFoldableProperties
    , monoFoldableWithKeyProperties
    , monoKeyedProperties
    , monoTraversableProperties
    , monoTraversableWithKeyProperties
    , monoZipProperties
    , monoZipWithKeyProperties
    , monoidProperties
    , normalFormDataProperties
    , orderingProperties
    , semigroupProperties
    , showProperties
--    , textshowProperties
    , bitVectorProperties
    , bitVectorRankSelect
    , monoFunctorEquivelence
    , monoFoldableEquivelence
    , monoZipEquivelence
    ]


bitsTests :: TestTree
bitsTests = testGroup "Properties of Bits"
    [ QC.testProperty "∀ i ≥ 0, clearBit zeroBits i === zeroBits" zeroBitsAndClearBit
    , QC.testProperty "∀ i ≥ 0, setBit   zeroBits i === bit i" zeroBitsAndSetBit
    , QC.testProperty "∀ i ≥ 0, testBit  zeroBits i === False" zeroBitsAndTestBit
    ,    testCase     "         popCount zeroBits   === 0" zeroBitsAndPopCount
    , QC.testProperty "complement === omap not" complementOmapNot
    , QC.testProperty "(`setBit` i) === (.|. bit i)" setBitDefinition
    , QC.testProperty "(`clearBit` i) === (.&. complement (bit i))" clearBitDefinition
    , QC.testProperty "(`complementBit` i) === (`xor` bit i)" complementBitDefinition
    , QC.testProperty "(`testBit` i) . (`setBit` n)" testBitAndSetBit
    , QC.testProperty "not  . (`testBit` i) . (`clearBit` i)" testBitAndClearBit
    , QC.testProperty "(`shiftL`  i) === (`shift`   i)" leftShiftPositiveShift
    , QC.testProperty "(`shiftR`  i) === (`shift`  -i)" rightShiftNegativeShift
    , QC.testProperty "(`rotateL` i) === (`rotate`  i)" leftRotatePositiveRotate
    , QC.testProperty "(`rotateR` i) === (`rotate` -i)" rightRotateNegativeRotate
    , QC.testProperty "(`rotateR` i) . (`rotateL` i) === id" leftRightRotateIdentity
    , QC.testProperty "(`rotateL` i) . (`rotateR` i) === id" rightLeftRotateIdentity
    ]
  where
    zeroBitsAndClearBit :: NonNegative Int -> Property
    zeroBitsAndClearBit (NonNegative i) =
        clearBit (zeroBits :: BitVector) i === zeroBits

    zeroBitsAndSetBit :: NonNegative Int -> Property
    zeroBitsAndSetBit (NonNegative i) =
        setBit   (zeroBits :: BitVector) i === bit i

    zeroBitsAndTestBit :: NonNegative Int -> Property
    zeroBitsAndTestBit (NonNegative i) =
        testBit  (zeroBits :: BitVector) i === False

    zeroBitsAndPopCount :: Assertion
    zeroBitsAndPopCount =
        popCount (zeroBits :: BitVector) @?= 0

    complementOmapNot :: BitVector -> Property
    complementOmapNot bv =
        complement bv === omap not bv

    setBitDefinition :: NonNegative Int -> BitVector -> Property
    setBitDefinition (NonNegative i) bv =
        bv `setBit` i === bv .|. bit i

    clearBitDefinition :: NonNegative Int -> BitVector -> Property
    clearBitDefinition (NonNegative i) bv =
        i < (fromEnum . dimension) bv -=>
          (bv `clearBit` i === bv .&. complement  (zed .|. bit i))
      where
        zed = fromNumber (dimension bv) (0 :: Integer)

    complementBitDefinition :: NonNegative Int -> BitVector -> Property
    complementBitDefinition (NonNegative i) bv =
        bv `complementBit` i === bv `xor` bit i

    testBitAndSetBit :: NonNegative Int -> BitVector -> Bool
    testBitAndSetBit (NonNegative i) =
        (`testBit` i) . (`setBit` i)

    testBitAndClearBit :: NonNegative Int -> BitVector -> Bool
    testBitAndClearBit (NonNegative i) =
        not  . (`testBit` i) . (`clearBit` i)

    leftShiftPositiveShift :: NonNegative Int -> BitVector -> Property
    leftShiftPositiveShift (NonNegative i) bv =
        bv `shiftL` i === bv `shift` i
        
    rightShiftNegativeShift :: NonNegative Int -> BitVector -> Property
    rightShiftNegativeShift (NonNegative i) bv =
        bv `shiftR` i === bv `shift` (-i)
        
    leftRotatePositiveRotate :: NonNegative Int -> BitVector -> Property
    leftRotatePositiveRotate (NonNegative i) bv =
        bv `rotateL` i === bv `rotate` i

    rightRotateNegativeRotate :: NonNegative Int -> BitVector -> Property
    rightRotateNegativeRotate (NonNegative i) bv =
        bv `rotateR` i === bv `rotate` (-i)
       
    leftRightRotateIdentity :: NonNegative Int -> BitVector -> Property
    leftRightRotateIdentity (NonNegative i) bv =
        ((`rotateR` i) . (`rotateL` i)) bv === bv

    rightLeftRotateIdentity :: NonNegative Int -> BitVector -> Property
    rightLeftRotateIdentity (NonNegative i) bv =
        ((`rotateL` i) . (`rotateR` i)) bv === bv


finiteBitsTests :: TestTree
finiteBitsTests = testGroup "Properties of FiniteBits"
    [ QC.testProperty "bitSize === finiteBitSize" finiteBitSizeIsBitSize
    , QC.testProperty "bitSizeMaybe === Just . finiteBitSize" finiteBitSizeIsBitSizeMaybe
    , QC.testProperty "dimension === finiteBitSize" finiteBitSizeIsDimension
    , QC.testProperty "countLeadingZeros <= finiteBitSize" finiteBitSizeIsGreaterThanLeadingZeros
    , QC.testProperty "countTrailingZeros <= finiteBitSize" finiteBitSizeIsGreaterThanTrailingZeros
    , QC.testProperty "length . toBits === finiteBitSize" finiteBitSizeIsBitLength
    , QC.testProperty "length . takeWhile not === countLeadingZeros . fromBits" countLeadingZeroAndFromBits
    , QC.testProperty "length . takeWhile not . toBits === countLeadingZeros" countLeadingZeroAndToBits
    , QC.testProperty "length . takeWhile not . reverse === countTrailingZeros . fromBits" countTrailingZeroAndFromBits
    , QC.testProperty "length . takeWhile not . reverse . toBits === countTrailingZeros" countTrailingZeroAndToBits
    ]
  where
    finiteBitSizeIsBitSize :: BitVector -> Property
    finiteBitSizeIsBitSize bv =
        bitSize bv === finiteBitSize bv

    finiteBitSizeIsBitSizeMaybe :: BitVector -> Property
    finiteBitSizeIsBitSizeMaybe bv =
        bitSizeMaybe bv === (Just . finiteBitSize) bv
    
    finiteBitSizeIsDimension :: BitVector -> Property
    finiteBitSizeIsDimension bv =
        (fromEnum . dimension) bv === finiteBitSize bv

    finiteBitSizeIsGreaterThanLeadingZeros :: BitVector -> Bool
    finiteBitSizeIsGreaterThanLeadingZeros bv =
        countLeadingZeros bv <= finiteBitSize bv

    finiteBitSizeIsGreaterThanTrailingZeros :: BitVector -> Bool
    finiteBitSizeIsGreaterThanTrailingZeros bv =
        countTrailingZeros bv <= finiteBitSize bv

    finiteBitSizeIsBitLength :: BitVector -> Property
    finiteBitSizeIsBitLength bv =
        (length . toBits) bv === finiteBitSize bv

    countLeadingZeroAndFromBits :: [Bool] -> Property
    countLeadingZeroAndFromBits bs =
        (length . takeWhile not) bs === (countLeadingZeros . fromBits) bs

    countLeadingZeroAndToBits :: BitVector -> Property
    countLeadingZeroAndToBits bv =
        (length . takeWhile not . toBits) bv === countLeadingZeros bv

    countTrailingZeroAndFromBits :: [Bool] -> Property
    countTrailingZeroAndFromBits bs =
        (length . takeWhile not . reverse) bs === (countTrailingZeros . fromBits) bs

    countTrailingZeroAndToBits :: BitVector -> Property
    countTrailingZeroAndToBits bv =
       (length . takeWhile not . reverse . toBits) bv === countTrailingZeros bv


hashableTests :: TestTree
hashableTests = testGroup "Properties of Hashable"
    [ localOption (QuickCheckTests 10000)
        $ QC.testProperty "a == b -=> (hashWithSalt a) === (hashWithSalt b)" differentSaltsDifferentHashes
    ]
  where
    differentSaltsDifferentHashes :: BitVector -> Int -> Int -> Property
    differentSaltsDifferentHashes bv salt1 salt2 =
        salt1 /= salt2 -=> hashWithSalt salt1 bv /= hashWithSalt salt2 bv
 


monoAdjustableProperties :: TestTree
monoAdjustableProperties = testGroup "Properites of a MonoAdjustable"
    [ QC.testProperty "oadjust id k === id" oadjustId
    , QC.testProperty "oadjust (f . g) k === oadjust f k . oadjust g k" oadjustComposition
    , QC.testProperty "oadjust f k === omapWithKey (\\i -> if i == k then f else id)" omapConditionality
    , QC.testProperty "oreplace k v === oreplace k v . oadjust f k" oreplaceNullification
    , QC.testProperty "oreplace (f v) k === oadjust f k . oreplace k v" oreplaceApplication
    ]
  where
    oadjustId :: Word -> BitVector -> Property
    oadjustId k bv = 
        oadjust id k bv === bv

    oadjustComposition :: Blind (Bool -> Bool) -> Blind (Bool -> Bool) -> Word -> BitVector -> Property
    oadjustComposition (Blind f) (Blind g) k bv =
        oadjust (f . g) k bv === (oadjust f k . oadjust g k) bv

    omapConditionality :: Blind (Bool -> Bool) -> Word -> BitVector -> Property
    omapConditionality (Blind f) k bv =
        oadjust f k bv === omapWithKey (\i -> if i == k then f else id) bv

    oreplaceNullification :: Blind (Bool -> Bool) -> Word -> Bool -> BitVector -> Property
    oreplaceNullification (Blind f) k v bv =
        oreplace k v bv === (oreplace k v . oadjust f k) bv

    oreplaceApplication :: Blind (Bool -> Bool) -> Word -> Bool -> BitVector -> Property
    oreplaceApplication (Blind f) k v bv =
        oreplace k (f v) bv === (oadjust f k . oreplace k v) bv


monoFunctorProperties :: TestTree
monoFunctorProperties = testGroup "Properites of a MonoFunctor"
    [ QC.testProperty "omap id === id" omapId
    , QC.testProperty "omap (f . g)  === omap f . omap g" omapComposition
    ]
  where
    omapId :: BitVector -> Property
    omapId bv =
        omap id bv === bv

    omapComposition :: Blind (Bool -> Bool) -> Blind (Bool -> Bool) -> BitVector -> Property
    omapComposition (Blind f) (Blind g) bv =
        omap (f . g) bv ===  (omap f . omap g) bv


monoFoldableProperties :: TestTree
monoFoldableProperties = testGroup "Properties of MonoFoldable"
    [ QC.testProperty "ofoldr f z t === appEndo (ofoldMap (Endo . f) t ) z" testFoldrFoldMap
    , QC.testProperty "ofoldl' f z t === appEndo (getDual (ofoldMap (Dual . Endo . flip f) t)) z" testFoldlFoldMap
    , QC.testProperty "ofoldr f z === ofoldr f z . otoList" testFoldr
    , QC.testProperty "ofoldl' f z === ofoldl' f z . otoList" testFoldl
    , QC.testProperty "ofoldr1Ex f === foldr1 f . otoList" testFoldr1
    , QC.testProperty "ofoldl1Ex' f === foldl1 f . otoList" testFoldl1
    , QC.testProperty "oall f === getAll . ofoldMap (All . f)" testAll
    , QC.testProperty "oany f === getAny . ofoldMap (Any . f)" testAny
    , QC.testProperty "olength === length . otoList" testLength
    , QC.testProperty "onull === (0 ==) . olength" testNull
    , QC.testProperty "headEx === getFirst . ofoldMap1Ex First" testHead
    , QC.testProperty "lastEx === getLast . ofoldMap1Ex Last" testTail
    , QC.testProperty "oelem e /== onotElem e" testInclusionConsistency
    ]
  where
    testFoldrFoldMap :: Blind (Bool -> Word -> Word) -> Word -> BitVector -> Property
    testFoldrFoldMap (Blind f) z bv =
        ofoldr f z bv === appEndo (ofoldMap (Endo . f) bv) z

    testFoldlFoldMap :: Blind (Word -> Bool -> Word) -> Word -> BitVector -> Property
    testFoldlFoldMap (Blind f) z bv =
        ofoldl' f z bv === appEndo (getDual (ofoldMap (Dual . Endo . flip f) bv)) z

    testFoldr :: Blind (Bool -> Word -> Word) -> Word -> BitVector -> Property
    testFoldr (Blind f) z bv =
        ofoldr f z bv === (ofoldr f z . otoList) bv

    testFoldl :: Blind (Word -> Bool -> Word) -> Word -> BitVector -> Property
    testFoldl (Blind f) z bv =
        ofoldl' f z bv === (ofoldl' f z . otoList) bv

--    testFoldr1 :: Blind (Bool -> Bool -> Bool) -> BitVector -> Property
    testFoldr1 :: BinaryLogicalOperator -> BitVector -> Property
--    testFoldr1 (Blind f) bv =
    testFoldr1 x bv =
        (not . onull) bv  -=> ofoldr1Ex f bv === (foldr1 f . otoList) bv
      where
        f = getBinaryLogicalOperator x

    testFoldl1 :: Blind (Bool -> Bool -> Bool) -> BitVector -> Property
    testFoldl1 (Blind f) bv =
        (not . onull) bv  -=> ofoldl1Ex' f bv === (foldl1 f . otoList) bv

    testAll :: Blind (Bool -> Bool) -> BitVector -> Property
    testAll (Blind f) bv =
        oall f bv === (getAll . ofoldMap (All . f)) bv

    testAny :: Blind (Bool -> Bool) -> BitVector -> Property
    testAny (Blind f) bv =
        oany f bv === (getAny . ofoldMap (Any . f)) bv

    testLength :: BitVector -> Property
    testLength bv =
        olength bv === (length . otoList) bv

    testNull :: BitVector -> Property
    testNull bv =
        onull bv === ((0 ==) . olength) bv

    testHead :: BitVector -> Property
    testHead bv =
        (not . onull) bv -=> headEx bv === (getFirst . ofoldMap1Ex First) bv

    testTail :: BitVector -> Property
    testTail bv =
        (not . onull) bv -=> lastEx bv === (getLast . ofoldMap1Ex Last) bv

    testInclusionConsistency :: (Bool, BitVector) -> Property
    testInclusionConsistency (e, bv) =
        oelem e bv === (not . onotElem e) bv


monoFoldableWithKeyProperties :: TestTree
monoFoldableWithKeyProperties = testGroup "Properties of MonoFoldableWithKey"
    [ QC.testProperty "otoKeyedList === zip [0..] . otoList" testNaturalKeyedList
    , QC.testProperty "ofoldMapWithKey (const f) === ofoldMap f" testConstantFoldMap
    , QC.testProperty "ofoldrWithKey (const f) === ofoldr f" testConstantFoldr
    , QC.testProperty "ofoldlWithKey (const . f) === ofoldl f" testConstantFoldl
    , QC.testProperty "ofoldMapWithKey f === foldMap (uncurry f) . otoKeyedList" testUncurriedFoldMap
    , QC.testProperty "ofoldrWithKey f === foldr (uncurry f) . otoKeyedList" testUncurriedFoldr
    , QC.testProperty "ofoldlWithKey f === foldl (uncurry . f) . otoKeyedList" testUncurriedFoldl
    ]
  where
    testNaturalKeyedList :: BitVector -> Property
    testNaturalKeyedList bv =
        otoKeyedList bv === (zip [0..] . otoList) bv

    testConstantFoldMap :: Blind (Bool -> [Word]) -> BitVector -> Property
    testConstantFoldMap (Blind f) bv =
        ofoldMapWithKey (const f) bv === ofoldMap f bv

    testConstantFoldr :: Blind (Bool -> Word -> Word) -> Word -> BitVector -> Property
    testConstantFoldr (Blind f) e bv =
        ofoldrWithKey (const f) e bv === ofoldr f e bv

    testConstantFoldl :: Blind (Word -> Bool -> Word) -> Word -> BitVector -> Property
    testConstantFoldl (Blind f) e bv =
        ofoldlWithKey (const . f) e bv === ofoldl' f e bv

    testUncurriedFoldMap :: Blind (Word -> Bool -> [Word]) -> BitVector -> Property
    testUncurriedFoldMap (Blind f) bv =
        ofoldMapWithKey f bv === (foldMap (uncurry f) . otoKeyedList) bv

    testUncurriedFoldr :: Blind (Word -> Bool -> Word -> Word) -> Word -> BitVector -> Property
    testUncurriedFoldr (Blind f) e bv =
        ofoldrWithKey f e bv === (foldr (uncurry f) e . otoKeyedList) bv

    testUncurriedFoldl :: Blind (Word -> Word -> Bool -> Word) -> Word -> BitVector -> Property
    testUncurriedFoldl (Blind f) e bv =
        ofoldlWithKey f e bv === (foldl (uncurry . f) e . otoKeyedList) bv


monoKeyedProperties :: TestTree
monoKeyedProperties = testGroup "Properites of a MonoKeyed"
    [ QC.testProperty "omapWithKey (const id) === id" omapId
    , QC.testProperty "omapWithKey (\\k -> f k . g k)  === omapWithKey f . omapWithKey g" omapComposition
    ]
  where
    omapId :: BitVector -> Property
    omapId bv =
        omapWithKey (const id) bv === bv

    omapComposition :: Blind (Word -> Bool -> Bool) -> Blind (Word -> Bool -> Bool) -> BitVector -> Property
    omapComposition (Blind f) (Blind g) bv =
        omapWithKey (\k -> f k . g k) bv === (omapWithKey f . omapWithKey g) bv


monoTraversableProperties :: TestTree
monoTraversableProperties = testGroup "Properties of MonoTraversable"
    [ QC.testProperty "t . otraverse f === otraverse (t . f)" testNaturality
    , QC.testProperty "otraverse Identity === Identity" testIdentity
    , QC.testProperty "otraverse (Compose . fmap g . f) === Compose . fmap (otraverse g) . otraverse f" testComposition
    , QC.testProperty "otraverse === omapM" testDefinitionEquality
    ]
  where
    testNaturality :: Blind (Bool -> [Bool]) -> BitVector -> Property
    testNaturality (Blind f) bv =
        (headMay . otraverse f) bv === otraverse (headMay . f) bv

    testIdentity :: BitVector -> Property
    testIdentity bv =
        otraverse Identity bv === Identity bv

    testComposition :: Blind (Bool -> Either Word Bool) -> Blind (Bool -> Maybe Bool) -> BitVector -> Property
    testComposition (Blind f) (Blind g) bv =
        otraverse (Compose . fmap g . f) bv === (Compose . fmap (otraverse g) . otraverse f) bv

    testDefinitionEquality :: Blind (Bool -> Maybe Bool) -> BitVector -> Property
    testDefinitionEquality (Blind f) bv =
        otraverse f bv === omapM f bv


monoTraversableWithKeyProperties :: TestTree
monoTraversableWithKeyProperties = testGroup "Properties of MonoTraversableWithKey"
    [ QC.testProperty "t . otraverseWithKey f === otraverseWithKey (\\k -> t . f k)" testNaturality
    , QC.testProperty "otraverseWithKey (const Identity) === Identity" testIdentity
    , QC.testProperty "otraverseWithKey (\\k -> Compose . fmap (g k) . f k) === Compose . fmap (otraverseWithKey g) . otraverseWithKey f" testComposition
    , QC.testProperty "otraverseWithKey === omapWithKeyM" testDefinitionEquality
    ]
  where
    testNaturality ::  Blind (Word -> Bool -> [Bool]) -> BitVector -> Property
    testNaturality (Blind f) bv =
        (headMay . otraverseWithKey f) bv === otraverseWithKey (\k -> headMay . f k) bv

    testIdentity :: BitVector -> Property
    testIdentity bv =
        otraverseWithKey (const Identity) bv === Identity bv

    testComposition :: Blind (Word -> Bool -> Either Word Bool) -> Blind (Word -> Bool -> Maybe Bool) -> BitVector -> Property
    testComposition (Blind f) (Blind g) bv =
        otraverseWithKey (\k -> Compose . fmap (g k) . f k) bv === (Compose . fmap (otraverseWithKey g) . otraverseWithKey f) bv

    testDefinitionEquality :: Blind (Word -> Bool -> Maybe Bool) -> BitVector -> Property
    testDefinitionEquality (Blind f) bv =
        otraverseWithKey f bv === omapWithKeyM f bv


monoZipProperties :: TestTree
monoZipProperties = testGroup "Properites of a MonoZip"
    [ QC.testProperty "ozipWith const u u === ozipWith (flip const) u u === u" ozipWithConst
    , QC.testProperty "ozipWith (flip f) x y === ozipWith f y x" ozipWithTransposition
    , QC.testProperty "ozipWith (\\a b -> f (g a) (h b)) x y === ozipWith f (omap g x) (omap h y)" ozipWithComposition
    ]
  where
    ozipWithConst :: BitVector -> Property
    ozipWithConst u =
        ozipWith const u u === u .&&. ozipWith (flip const) u u === u

    ozipWithTransposition :: Blind (Bool -> Bool -> Bool) -> BitVector -> BitVector -> Property
    ozipWithTransposition (Blind f) x y =
        ozipWith (flip f) x y === ozipWith f y x

    ozipWithComposition
      :: Blind (Bool -> Bool -> Bool)
      -> Blind (Bool -> Bool)
      -> Blind (Bool -> Bool) -> BitVector -> BitVector -> Property
    ozipWithComposition (Blind f) (Blind g) (Blind h) x y =
        ozipWith (\a b -> f (g a) (h b)) x y === ozipWith f (omap g x) (omap h y)


monoZipWithKeyProperties :: TestTree
monoZipWithKeyProperties = testGroup "Properites of a MonoZipWithKey"
    [ QC.testProperty "ozipWithKey (const f) === ozipWith f" ozipWithKeyConst
    ]
  where
    ozipWithKeyConst :: Blind (Bool -> Bool -> Bool) -> BitVector -> BitVector -> Property
    ozipWithKeyConst (Blind f) x y =
        ozipWithKey (const f) x y === ozipWith f x y


monoidProperties :: TestTree
monoidProperties = testGroup "Properties of a Monoid"
    [ QC.testProperty "left identity"   leftIdentity
    , QC.testProperty "right identity" rightIdentity
    , QC.testProperty "mempty is associative" operationAssocativity
    , QC.testProperty "mconcat === foldr (<>) mempty" foldableApplication
    ]
  where
    leftIdentity :: BitVector -> Property
    leftIdentity a =
        mempty `mappend` a === a

    rightIdentity :: BitVector -> Property
    rightIdentity a =
        a `mappend` mempty === a

    operationAssocativity :: BitVector -> BitVector -> BitVector -> Property
    operationAssocativity a b c =
        a `mappend` (b `mappend` c) === (a `mappend` b) `mappend` c

    foldableApplication :: [BitVector] -> Property
    foldableApplication bvs = 
        mconcat bvs === foldr mappend mempty bvs


normalFormDataProperties :: TestTree
normalFormDataProperties = testGroup "Properties of NFData"
    [ QC.testProperty "rnf result is finite" finiteReduction
    ]
  where
    finiteReduction :: BitVector -> Property
    finiteReduction bv =
        rnf bv === ()


orderingProperties :: TestTree
orderingProperties = testGroup "Properties of an Ordering"
    [ QC.testProperty "ordering preserves symetry"  symetry
    , QC.testProperty "ordering is transitive (total)" transitivity
    ]
  where
    symetry :: BitVector -> BitVector -> Bool
    symetry lhs rhs =
        case (lhs `compare` rhs, rhs `compare` lhs) of
          (EQ, EQ) -> True
          (GT, LT) -> True
          (LT, GT) -> True
          _        -> False

    transitivity :: BitVector -> BitVector -> BitVector -> Property
    transitivity a b c = caseOne .||. caseTwo
      where
        caseOne = (a <= b && b <= c) -=> a <= c
        caseTwo = (a >= b && b >= c) -=> a >= c


semigroupProperties :: TestTree
semigroupProperties = testGroup "Properties of a Semigroup"
    [ localOption (QuickCheckTests 10000)
        $ QC.testProperty "(<>) is associative" operationAssocativity
    , QC.testProperty "sconcat === foldr1 (<>)" foldableApplication
    , QC.testProperty "stimes n === mconcat . replicate n" repeatedApplication
    ]
  where
    operationAssocativity :: BitVector -> BitVector -> BitVector -> Property
    operationAssocativity a b c =
        a <> (b <> c) === (a <> b) <> c

    foldableApplication :: NonEmptyList BitVector -> Property
    foldableApplication nel =
        sconcat bvs === foldr1 mappend bvs
      where
        -- We do this because there is currently no Arbitrary inctance for NonEmpty
        bvs = fromList $ getNonEmpty nel

    repeatedApplication :: NonNegative Int -> BitVector -> Property
    repeatedApplication (NonNegative i) bv =
        stimes i bv === (mconcat . replicate i) bv


showProperties :: TestTree
showProperties = testGroup "Properties of Show"
    [ QC.testProperty "show result is finite" finiteString
    , QC.testProperty "show result is non-null" nonNullString
    ]
  where
    finiteString :: BitVector -> Property
    finiteString bv =
        show bv === show bv 
    
    nonNullString :: BitVector -> Bool
    nonNullString =
        not . null . show


{-
textshowProperties :: TestTree
textshowProperties = testGroup "Properties of TextShow"
    [ QC.testProperty "textshow and show result agree" textshowCoherence
    ]
  where
    textshowCoherence :: BitVector -> Property
    textshowCoherence bv =
        (toString . showb $ bv) === show bv
-}


bitVectorProperties :: TestTree
bitVectorProperties = testGroup "BitVector properties"
    [ QC.testProperty "otoList === toBits" otoListTest
    , QC.testProperty "dimension === length . toBits" dimensionAndToBits
    , QC.testProperty "dimension === finiteBitSize" dimensionAndFiniteBitSize
    , QC.testProperty "fromBits . toBits === id" toBitsFromBits
    ,    testCase     "isZeroVector zeroBits" zeroBitsIsZeroVector
    , QC.testProperty "isZeroVector === (0 ==) . popCount" popCountAndZeroVector
    , QC.testProperty "isZeroVector === all not . toBits" zeroVectorAndAllBitsOff
    , QC.testProperty "(0 ==) . toUnsignedNumber -=> isZeroVector" toUnsignedNumImpliesZeroVector
    , QC.testProperty "toSignedNumber . fromNumber === id" bitVectorUnsignedNumIdentity
    , QC.testProperty "isSigned == const False" noSignBitVector
-- For an unknown reason, this test case causes GHC to panic!
    , QC.testProperty "i >  j -=> subRange (i,j) === const zeroBits" badSubRangeEmptyResult
    , QC.testProperty "i <= j -=> dimension . subRange (i,j) === const (j - i + 1)" subRangeFixedDimension
    ]
  where
    otoListTest :: BitVector -> Property
    otoListTest bv =
        otoList bv === toBits bv

    dimensionAndToBits :: BitVector -> Property
    dimensionAndToBits bv =
        (fromEnum . dimension) bv === (length . toBits) bv

    dimensionAndFiniteBitSize :: BitVector -> Property
    dimensionAndFiniteBitSize bv =
        (fromEnum . dimension) bv === finiteBitSize bv

    toBitsFromBits :: BitVector -> Property
    toBitsFromBits bv =
        (fromBits . toBits) bv === bv

    zeroBitsIsZeroVector :: Assertion
    zeroBitsIsZeroVector =
        assertBool "zeroBits is not a 'zero vector'" $ isZeroVector zeroBits

    popCountAndZeroVector :: BitVector -> Property
    popCountAndZeroVector bv =
        isZeroVector bv === ((0 ==) . popCount) bv

    zeroVectorAndAllBitsOff :: BitVector -> Property
    zeroVectorAndAllBitsOff bv =
        isZeroVector bv === (all not . toBits) bv

    toUnsignedNumImpliesZeroVector :: BitVector -> Property
    toUnsignedNumImpliesZeroVector bv =
        ((0 ==) . (toUnsignedNumber :: BitVector -> Integer)) bv -=> isZeroVector bv

    bitVectorUnsignedNumIdentity :: Integer -> Property
    bitVectorUnsignedNumIdentity num =
        (toSignedNumber . fromNumber width) num === num
      where
        width = succ . succ . ceiling . logBase (2.0 :: Double) . fromIntegral $ abs num

    noSignBitVector :: BitVector -> Property
    noSignBitVector bv =
        isSigned bv === False

    badSubRangeEmptyResult :: (Word, Word) -> BitVector -> Property
    badSubRangeEmptyResult range@(lower, upper) bv =
        lower > upper -=> subRange range bv === zeroBits

    subRangeFixedDimension :: (NonNegative Int, NonNegative Int) -> BitVector -> Property
    subRangeFixedDimension (NonNegative lowerI, NonNegative upperI) bv =
        lower <= upper -=> dimension (subRange (lower, upper) bv) === upper - lower + 1
      where
        lower = toEnum lowerI
        upper = toEnum upperI


bitVectorRankSelect :: TestTree
bitVectorRankSelect = testGroup "BitVector rank/select"
    [ QC.testProperty "select (bit i) 0 === i" selectBitValue
    , QC.testProperty "select (bit x .|. bit y) 0 === min (select (bit x) 0) (select (bit y) 0)" selectBitOr
    , QC.testProperty "rank (bit x .|. bit y) (max x y + 1) === rank (bit x) (x+1) + rank (bit y) (y+1)" rankBitOr
    , QC.testProperty "rank (bit i) (i+1) === i" rankBitValue
    , QC.testProperty "rank <$> id <*> dimension === popCount" rankPopCount
    , QC.testProperty "rank bv i === length . filter id . take i . toBits bv" rankToBits
    , QC.testProperty "rank bv (select bv i) === i" rankSelectMinDef
    ]
  where
    selectBitValue :: NonNegative Int -> Property
    selectBitValue (NonNegative x) =
        select (bit x) 0 === Just (toEnum x)

    selectBitOr :: NonNegative Int -> NonNegative Int -> Property
    selectBitOr (NonNegative x) (NonNegative y) =
        select (bit x .|. bit y) 0 === min (select (bit x) 0) (select (bit y) 0)

    rankBitValue :: NonNegative Word -> Property
    rankBitValue (NonNegative x) =
        rank (bit (fromEnum x)) (x + 1) === 1

    rankBitOr :: NonNegative Int -> NonNegative Int -> Property
    rankBitOr (NonNegative x) (NonNegative y) =
        x /= y -=>
          rank (bit x .|. bit y) z' === rank (bit x) (x' + 1) + rank (bit y) (y' + 1)
      where
        x' = toEnum x
        y' = toEnum y
        z' = max x' y' + 1

    rankPopCount :: BitVector -> Property
    rankPopCount bv =
        (rank <$> id <*> dimension) bv === toEnum (popCount bv)

    rankToBits :: BitVector -> NonNegative Word -> Property
    rankToBits bv (NonNegative x) =
        rank bv x === (toEnum . length . filter id . take (fromEnum x) . toBits) bv

    rankSelectMinDef :: BitVector -> NonNegative Word -> Property
    rankSelectMinDef bv (NonNegative x) =
        let idx = select bv x
            k   = fromJust idx
        in  idx === Nothing .||. rank bv k === x
        

monoFunctorEquivelence :: TestTree
monoFunctorEquivelence = testGroup "Equivelence of a MonoFunctor"
    [ SC.testProperty "omap f === fromBits . map f . toBits" $ forAll omapOptimizationIsValid
    ]
  where
    omapOptimizationIsValid :: (Bool -> Bool, VisualBitVector) -> Bool
    omapOptimizationIsValid (f, y) = omap f bv == (fromBits . map f . toBits) bv
      where
        bv = getBitVector y


monoFoldableEquivelence :: TestTree
monoFoldableEquivelence = testGroup "Equivelence of a MonoFoldable"
    [ SC.testProperty "oall f === all f . otoList"              $ forAll oallOptimizationIsValid
    , SC.testProperty "oany f === any f . otoList"              $ forAll oanyOptimizationIsValid
    , SC.testProperty "ofoldr1Ex  f === foldr1 f . otoList"     $ forAll ofoldr1ExOptimizationIsValid
    , SC.testProperty "ofoldl1Ex' f === foldl1 f . otoList"     $ forAll ofoldl1ExOptimizationIsValid
    , SC.testProperty "headEx === head . otoList"               $ forAll headExOptimizationIsValid
    , SC.testProperty "lastEx === last . otoList"               $ forAll lastExOptimizationIsValid
    , SC.testProperty "maximumByEx f === maximumBy f . otoList" $ forAll maximumByExOptimizationIsValid
    , SC.testProperty "minimumByEx f === minimumBy f . otoList" $ forAll minimumByExOptimizationIsValid
    , SC.testProperty "oelem e === oelem e . otoList"           $ forAll oelemOptimizationIsValid
    , SC.testProperty "onotElem e === onotElem e . otoList"     $ forAll onotElemOptimizationIsValid
    ]
  where
    oallOptimizationIsValid :: (UnaryLogicalOperator, VisualBitVector) -> Bool
    oallOptimizationIsValid (y, x) = oall op bv == (all op . otoList) bv
      where
        bv = getBitVector x
        op = getUnaryLogicalOperator y

    oanyOptimizationIsValid :: (UnaryLogicalOperator, VisualBitVector) -> Bool
    oanyOptimizationIsValid (y, x) = oany op bv == (any op . otoList) bv
      where
        bv = getBitVector x
        op = getUnaryLogicalOperator y

    ofoldr1ExOptimizationIsValid :: (BinaryLogicalOperator, VisualBitVector) -> Bool
    ofoldr1ExOptimizationIsValid (y, x) =
        isZeroVector bv || ofoldr1Ex op bv == (foldr1 op . otoList) bv
      where
        bv = getBitVector x
        op = getBinaryLogicalOperator  y

    ofoldl1ExOptimizationIsValid :: (BinaryLogicalOperator, VisualBitVector) -> Bool
    ofoldl1ExOptimizationIsValid (y, x) =
        isZeroVector bv || ofoldl1Ex' op bv == (foldl1 op . otoList) bv
      where
        bv = getBitVector x
        op = getBinaryLogicalOperator  y

    headExOptimizationIsValid :: VisualBitVector -> Bool
    headExOptimizationIsValid x =
        isZeroVector bv || headEx bv == (head . otoList) bv
      where
        bv = getBitVector x

    lastExOptimizationIsValid :: VisualBitVector -> Bool
    lastExOptimizationIsValid x =
        isZeroVector bv || lastEx bv == (last . otoList) bv
      where
        bv = getBitVector x

    maximumByExOptimizationIsValid :: (VisualBitVector, ComparisonOperator) -> Bool
    maximumByExOptimizationIsValid (x, y) =
        isZeroVector bv || maximumByEx op bv == (maximumBy op . otoList) bv
      where
        bv = getBitVector  x
        op = getComparator y

    minimumByExOptimizationIsValid :: (VisualBitVector, ComparisonOperator) -> Bool
    minimumByExOptimizationIsValid (x, y) =
        isZeroVector bv || minimumByEx op bv == (minimumBy op . otoList) bv
      where
        bv = getBitVector  x
        op = getComparator y

    oelemOptimizationIsValid :: (VisualBitVector, Bool) -> Bool
    oelemOptimizationIsValid (x, e) = oelem e bv == (oelem e . otoList) bv
      where
        bv = getBitVector x

    onotElemOptimizationIsValid :: (VisualBitVector, Bool) -> Bool
    onotElemOptimizationIsValid (x, e) = onotElem e bv == (onotElem e . otoList) bv
      where
        bv = getBitVector x


monoZipEquivelence :: TestTree
monoZipEquivelence = testGroup "Equivelence of a MonoZip"
    [ SC.testProperty "ozipWith f x === fromBits . zipWith f . (toBits x) . toBits" $ forAll omapOptimizationIsValid
    ]
  where
    omapOptimizationIsValid :: (BinaryLogicalOperator, VisualBitVectorSmall, VisualBitVectorSmall) -> Bool
    omapOptimizationIsValid (f, x, y) = ozipWith op lhs rhs == (fromBits . zipWith op (toBits lhs) . toBits) rhs
      where
        op  = getBinaryLogicalOperator f
        lhs = getBitVector x
        rhs = getBitVector y

{-
infixr 0 ===>
(===>) :: QC.Testable prop => Bool -> prop -> Property
False ===> _ = property True
True  ===> p = property p
-}
