{-# LANGUAGE FlexibleInstances #-}

-- We apply this to suppress the deprecated warning cause by calls to 'bitSize'
-- If there is a more fine-grained way to supress this warning without suppressing
-- deprecated warnings for the whole module, we should do that instead.
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Main ( main ) where

import           Control.DeepSeq
import           Data.Bits
import           Data.BitVector.LittleEndian
import           Data.BitVector.Visual
import           Data.Foldable
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Hashable
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Monoid ()
import           Data.MonoTraversable
import           Data.Semigroup
import           Operator.Binary.Comparison
import           Operator.Binary.Logical
import           Operator.Unary.Logical
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck hiding ((.&.), forAll, testProperty)
import qualified Test.Tasty.QuickCheck as QC
import           Test.Tasty.SmallCheck hiding ((===), (==>), Property, testProperty)
import qualified Test.Tasty.SmallCheck as SC


main :: IO ()
main = defaultMain testSuite


testSuite :: TestTree
testSuite = testGroup "BitVector tests"
    [ bitsTests
    , finiteBitsTests
    , hashableTests
    , monoFunctorProperties
    , monoFoldableProperties
    , monoidProperties
    , monoTraversableProperties
    , normalFormDataProperties
    , orderingProperties
    , semigroupProperties
    , showProperties
    , bitVectorProperties
    , monoFunctorEquivelence
    , monoFoldableEquivelence
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
        i < (fromEnum . dimension) bv ==>
          (bv `clearBit` i === bv .&. complement  (zed .|. bit i))
      where
        zed = fromNumber (dimension bv) (0 :: Integer)

    complementBitDefinition :: NonNegative Int -> BitVector -> Property
    complementBitDefinition (NonNegative i) bv =
        bv `complementBit` i === bv `xor` bit i

    testBitAndSetBit :: NonNegative Int -> BitVector -> Bool
    testBitAndSetBit (NonNegative i) bv =
        ((`testBit` i) . (`setBit` i)) bv

    testBitAndClearBit :: NonNegative Int -> BitVector -> Bool
    testBitAndClearBit (NonNegative i) bv =
        (not  . (`testBit` i) . (`clearBit` i)) bv

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
    , QC.testProperty "countLeadingZeros <= finiteBitSize" finiteBitSizeIsGreaterThanLeadingZeros
    , QC.testProperty "CountTrailingZeros <= finiteBitSize" finiteBitSizeIsGreaterThanTrailingZeros
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
        $ QC.testProperty "a == b ==> (hashWithSalt a) === (hashWithSalt b)" differentSaltsDifferentHashes
    ]
  where
    differentSaltsDifferentHashes :: BitVector -> Int -> Int -> Property
    differentSaltsDifferentHashes bv salt1 salt2 =
        salt1 /= salt2 ==> (hashWithSalt salt1 bv) /= (hashWithSalt salt2 bv)
 
    
monoFunctorProperties :: TestTree
monoFunctorProperties = testGroup "Properites of a MonoFunctor"
    [ QC.testProperty "omap id === id" omapId
    , QC.testProperty "omap (f . g)  === omap f . omap g" omapComposition
    ]
  where
    omapId :: BitVector -> Property
    omapId bv =
        omap id bv === id bv

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
        (not . onull) bv  ==> ofoldr1Ex f bv === (foldr1 f . otoList) bv
      where
        f = getBinaryLogicalOperator x

    testFoldl1 :: Blind (Bool -> Bool -> Bool) -> BitVector -> Property
    testFoldl1 (Blind f) bv =
        (not . onull) bv  ==> ofoldl1Ex' f bv === (foldl1 f . otoList) bv

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
        (not . onull) bv ==> headEx bv === (getFirst . ofoldMap1Ex First) bv

    testTail :: BitVector -> Property
    testTail bv =
        (not . onull) bv ==> lastEx bv === (getLast . ofoldMap1Ex Last) bv

    testInclusionConsistency :: (Bool, BitVector) -> Property
    testInclusionConsistency (e, bv) =
        oelem e bv === (not . onotElem e) bv


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
        caseOne = (a <= b && b <= c) ==> a <= c
        caseTwo = (a >= b && b >= c) ==> a >= c


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
        bvs = let x:xs = getNonEmpty nel
              in  x:|xs

    repeatedApplication :: (NonNegative Int) -> BitVector -> Property
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
    

bitVectorProperties :: TestTree
bitVectorProperties = testGroup "BitVector properties"
    [ QC.testProperty "otoList === toBits" otoListTest
    , QC.testProperty "dimension === length . toBits" dimensionAndToBits
    , QC.testProperty "dimension === finiteBitSize" dimensionAndFiniteBitSize
    , QC.testProperty "fromBits . toBits === id" toBitsFromBits
    ,    testCase     "isZeroVector zeroBits" zeroBitsIsZeroVector
    , QC.testProperty "isZeroVector === (0 ==) . popCount" popCountAndZeroVector
    , QC.testProperty "isZeroVector === all not . toBits" zeroVectorAndAllBitsOff
    , QC.testProperty "(0 ==) . toUnsignedNumber ==> isZeroVector" toUnsignedNumImpliesZeroVector
    , QC.testProperty "toSignedNumber . fromNumber === id" bitVectorUnsignedNumIdentity
    , QC.testProperty "isSigned == const False" noSignBitVector
-- For an unknown reason, this test case causes GHC to panic!
--    , QC.testProperty "i >  j ==> subRange (i,j) === const zeroBits" badSubRangeEmptyResult
    , QC.testProperty "i <= j ==> dimension . subRange (i,j) === const (j - i + 1)" subRangeFixedDimension
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
        ((0 ==) . (toUnsignedNumber :: BitVector -> Integer)) bv ==> isZeroVector bv

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
        lower > upper ==> subRange range bv === zeroBits

    subRangeFixedDimension :: (NonNegative Int, NonNegative Int) -> BitVector -> Property
    subRangeFixedDimension (NonNegative lowerI, NonNegative upperI) bv =
        lower <= upper ==> dimension (subRange (lower, upper) bv) === upper - lower + 1
      where
        lower = toEnum lowerI
        upper = toEnum upperI


monoFunctorEquivelence :: TestTree
monoFunctorEquivelence = testGroup "Equivelence of a MonoFunctor"
    [ SC.testProperty "omap f === fromBits . map f . toBits" $ forAll omapOptimizationIsValid
    ]
  where
    omapOptimizationIsValid :: (Bool -> Bool, VisualBitVector) -> Bool
    omapOptimizationIsValid (f, y) = (omap f) bv == (fromBits . map f . toBits) bv
      where
        bv = getBitVector y


monoFoldableEquivelence :: TestTree
monoFoldableEquivelence = testGroup "Equivelence of a MonoFoldable"
    [ SC.testProperty "oall f === all f . otoList"              $ forAll oallOptimizationIsValid
    , SC.testProperty "oany f === any f . otoList"              $ forAll oanyOptimizationIsValid
    , SC.testProperty "ofoldr1Ex f === foldr1 f . otoList"      $ forAll ofoldr1ExOptimizationIsValid
    , SC.testProperty "headEx === head . otoList"               $ forAll headExOptimizationIsValid
    , SC.testProperty "lastEx === last . otoList"               $ forAll lastExOptimizationIsValid
--    , SC.testProperty "maximumByEx f === maximumBy f . otoList" $ forAll maximumByExOptimizationIsValid
--    , SC.testProperty "minimumByEx f === minimumBy f . otoList" $ forAll minimumByExOptimizationIsValid
    , SC.testProperty "oelem e === oelem e . otoList"           $ forAll oelemOptimizationIsValid
    , SC.testProperty "onotElem e === onotElem e . otoList"     $ forAll onotElemOptimizationIsValid
    ]
  where
    oallOptimizationIsValid :: (UnaryLogicalOperator, VisualBitVector) -> Bool
    oallOptimizationIsValid (y, x) = (oall op) bv == (all op . otoList) bv
      where
        bv = getBitVector x
        op = getUnaryLogicalOperator y

    oanyOptimizationIsValid :: (UnaryLogicalOperator, VisualBitVector) -> Bool
    oanyOptimizationIsValid (y, x) = (oany op) bv == (any op . otoList) bv
      where
        bv = getBitVector x
        op = getUnaryLogicalOperator y

    ofoldr1ExOptimizationIsValid :: (BinaryLogicalOperator, VisualBitVector) -> Bool
    ofoldr1ExOptimizationIsValid (y, x) =
        isZeroVector bv || (ofoldr1Ex op) bv == (foldr1 op . otoList) bv
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
        isZeroVector bv || (maximumByEx op) bv == (maximumBy op . otoList) bv
      where
        bv = getBitVector  x
        op = getComparator y

    minimumByExOptimizationIsValid :: (VisualBitVector, ComparisonOperator) -> Bool
    minimumByExOptimizationIsValid (x, y) =
        isZeroVector bv || (minimumByEx op) bv == (minimumBy op . otoList) bv
      where
        bv = getBitVector  x
        op = getComparator y

    oelemOptimizationIsValid :: (VisualBitVector, Bool) -> Bool
    oelemOptimizationIsValid (x, e) = (oelem e) bv == (oelem e . otoList) bv
      where
        bv = getBitVector x

    onotElemOptimizationIsValid :: (VisualBitVector, Bool) -> Bool
    onotElemOptimizationIsValid (x, e) = (onotElem e) bv == (onotElem e . otoList) bv
      where
        bv = getBitVector x
