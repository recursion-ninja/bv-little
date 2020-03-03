{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.DeepSeq
import Criterion.Main
import Data.Bits
import Data.BitVector.LittleEndian
import Data.BitVector.LittleEndian.Instances ()
import Data.List (nubBy)
import Data.Hashable
import Data.MonoTraversable
import Operator.Binary.Logical
import Operator.Unary.Logical


main :: IO ()
main = defaultMain [ benchmarks ]


benchmarks :: Benchmark
benchmarks = bgroup "BitVector"
    [ toBitsBench
    , fromBitsBench
    , fromNumberBench
    , toSignedNumberBench
    , toUnsignedNumberBench
    , dimmensionBench
    , isZeroVectorBench
    , zeroPopCountBench
    , subRangeBench
    , bitsBench
    , finiteBitsBench
    , hashableBench
    , semigroupBench
    , monoFoldableBench
    ]


-- |
-- This number is the first 10-digit prime in e. It is used as a "no trick up my sleeve" arbitrary large number.
--
-- ceiling ( log_2 (prime) ) === 33
tinyNumber :: Integer
tinyNumber = 7427466391


-- |
-- This number is phi * 10^20. It is used as a "no trick up my sleeve" arbitrary large number.
--
-- ceiling ( log_2 (phi * 10^20) ) === 68
smallNumber :: Integer
smallNumber = 161803398874989484820


-- |
-- This number is e * 10^50. It is used as a "no trick up my sleeve" arbitrary large number.
--
-- ceiling ( log_2 (e * 10^50) ) === 168
mediumNumber :: Integer
mediumNumber = 271828182845904523536028747135266249775724709369995


-- |
-- This number is pi * 10^100. It is used as a "no trick up my sleeve" arbitrary large number.
--
-- ceiling ( log_2 (pi * 10^100) ) === 334
largeNumber :: Integer
largeNumber = 31415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679


-- |
-- This number is -1 * √2 * 10^200. It is used as a "no trick up my sleeve" arbitrary large negative number.
--
-- ceiling ( log_2 (√2 * 10^200) ) === 665
hugeNumber :: Integer
hugeNumber  = 14142135623730950488016887242096980785696718753769480731766797379907324784621070388503875343276415727350138462309122970249248360558507372126441214970999358314132226659275055927557999505011527820605715


toBitsBench :: Benchmark
toBitsBench = unaryBenchmark "toBitsNumber" toBits


fromBitsBench :: Benchmark
fromBitsBench = constantNumberTimeBenchmark "fromBits" id g
  where
    g int n = let !bitCount = fromEnum $ logBase2Word int
                  bitStream = force $ foldMap (\i -> [testBit n i]) [0 .. bitCount - 1]
              in  fromBits bitStream


fromNumberBench :: Benchmark
fromNumberBench = constantNumberTimeBenchmark "fromNumber" id g
  where
    g int = let !bitCount = logBase2Word int
            in  fromNumber bitCount


toSignedNumberBench :: Benchmark
toSignedNumberBench = unaryBenchmark "toSignedNumber" (toSignedNumber :: BitVector -> Integer)


toUnsignedNumberBench :: Benchmark
toUnsignedNumberBench = unaryBenchmark "toUnsignedNumber" (toUnsignedNumber :: BitVector -> Integer)


dimmensionBench :: Benchmark
dimmensionBench = constantNumberTimeBenchmark "dimmension" id g
  where
    g int _ = let !bitCount  = logBase2Word int
                  !bitVector = fromNumber bitCount int
              in  dimension bitVector
  

isZeroVectorBench :: Benchmark
isZeroVectorBench = constantNumberTimeBenchmark "isZeroVector" id g
  where
    g int _ = let !bitCount  = logBase2Word int
                  !bitVector = fromNumber bitCount int
              in  isZeroVector bitVector
  

zeroPopCountBench :: Benchmark
zeroPopCountBench = constantNumberTimeBenchmark "popCount is zero" id g
  where
    g int _ = let !bitCount  = logBase2Word int
                  !bitVector = fromNumber bitCount int
              in  ((0==) . popCount) bitVector
  

subRangeBench :: Benchmark
subRangeBench = constantNumberTimeBenchmark "subRange" id g
  where
    g int _ = let !bitCount   = logBase2Word int
                  !bitVector  = fromNumber bitCount int
                  !lowerBound = bitCount `div` 4
                  !upperBound = (bitCount * 3) `div` 4
              in  (lowerBound, upperBound) `subRange` bitVector
  

bitsBench :: Benchmark
bitsBench = bgroup "Bits"
    [   binaryBenchmark "(.|.)" (.|.)
    ,   binaryBenchmark "(.&.)" (.&.)
    ,   binaryBenchmark "xor"    xor
    ,    unaryBenchmark "complement"    complement
--    ,    unaryBenchmark "bitSize"       bitSize
    ,    unaryBenchmark "bitSizeMaybe"  bitSizeMaybe
    ,    unaryBenchmark "isSigned"      isSigned
    ,    unaryBenchmark "popCount"      popCount
    , indexingBenchmark "shift"         shift
    , indexingBenchmark "shiftL"        shiftL
    , indexingBenchmark "shiftR"        shiftR
    , indexingBenchmark "rotate"        rotate
    , indexingBenchmark "rotateL"       rotateL
    , indexingBenchmark "rotateR"       rotateR
    , indexingBenchmark "setBit"        setBit
    , indexingBenchmark "clearBit"      clearBit
    , indexingBenchmark "complementBit" complementBit
    , indexingBenchmark "testBit"       testBit
    ]


finiteBitsBench :: Benchmark
finiteBitsBench = bgroup "FiniteBits"
    [ unaryBenchmark "finiteBitSize"      finiteBitSize
    , unaryBenchmark "countLeadingZeros"  countLeadingZeros
    , unaryBenchmark "countTrailingZeros" countLeadingZeros
    ]


hashableBench :: Benchmark
hashableBench = bgroup "Hashable"
    [ unaryBenchmark    "hash" hash
    , indexingBenchmark "hashWithSalt" (flip hashWithSalt)
    ]
  

semigroupBench :: Benchmark
semigroupBench = bgroup "Semigroup"
    [ binaryBenchmark "(<>)" (<>)
    ]
  

monoFoldableBench :: Benchmark
monoFoldableBench = bgroup "MonoFoldable"
    [ fold1Benchmark "ofoldr1Ex"  ofoldr1Ex
    , fold1Benchmark "ofoldl1Ex'" ofoldl1Ex'
    ,   mapBenchmark "omap"       omap
    , queryBenchmark "oall"       oall 
    , queryBenchmark "oany"       oany 
    ]


constantNumberTimeBenchmark :: (NFData a, NFData b) => String -> (Integer -> a) -> (Integer -> a -> b) -> Benchmark
constantNumberTimeBenchmark  label f g = bgroup label $ generateBenchmark <$> magicNumbers
  where
    generateBenchmark (intLabel, intValue) = bench intLabel $ nf app target
      where
        !target    = force $ f intValue
        !app       = g intValue

    
unaryBenchmark :: NFData a => String -> (BitVector -> a) -> Benchmark
unaryBenchmark label f = bgroup label $ generateBenchmark <$> magicNumbers
  where
    generateBenchmark (intLabel, intValue) = bench intLabel $ nf f target
      where
        !target = bvGen intValue

    
binaryBenchmark :: NFData a => String -> (BitVector -> BitVector -> a) -> Benchmark
binaryBenchmark label op = bgroup label $ generateBenchmark <$> combinations
  where
    generateBenchmark (intLabel1, intValue1, intLabel2, intValue2) = bench message $ nf id target
      where
        message  = unwords [intLabel1, "`op`", intLabel2]
        !lhs     = bvGen intValue1
        !rhs     = bvGen intValue2
        target   = lhs `op` rhs
    combinations = [ (a,b,c,d) | (a,b) <- magicNumbers, (c,d) <- magicNumbers, b < d ]


indexingBenchmark :: NFData a => String -> (BitVector -> Int -> a) -> Benchmark
indexingBenchmark label op = bgroup label $ generateBenchmark <$> combinations
  where
    generateBenchmark (intLabel, intValue, idxLabel, idxValue) = bench message $ nf app target
      where
        message  = unwords [intLabel, "@", idxLabel <> ":" <> show idxValue]
        !target  = bvGen intValue
        app     = (`op` idxValue)

    combinations = do
        (a, b) <- magicNumbers
        let bitCount = fromEnum $ logBase2Word b
        (c, d) <- nubBy (\x y -> snd x == snd y) [("first", 0), ("middle", bitCount `div` 2), ("last", bitCount - 1)]
        let e = force (a,b,c,d)
        [e]


fold1Benchmark :: String -> ((Bool -> Bool -> Bool) -> BitVector -> Bool) -> Benchmark
fold1Benchmark label fold1Fun = bgroup label $ generateBenchmark <$> combinations
  where
    generateBenchmark (intLabel, intValue, lOp) = bench message $ nf id target
      where
        message  = unwords ["fold1", getBinaryLogicalSymbol lOp, intLabel]
        !op      = getBinaryLogicalOperator lOp
        !bv      = bvGen intValue
        target   = fold1Fun op bv
    combinations = [ (a,b, op) | (a,b) <- magicNumbers, op <- [minBound .. maxBound] ]


mapBenchmark :: String -> ((Bool -> Bool) -> BitVector -> BitVector) -> Benchmark
mapBenchmark label mapFun = bgroup label $ generateBenchmark <$> combinations
  where
    generateBenchmark (intLabel, intValue, lOp) = bench message $ nf id target
      where
        message  = unwords ["map", getUnaryLogicalSymbol lOp, intLabel]
        !op      = getUnaryLogicalOperator lOp
        !bv      = bvGen intValue
        target   = mapFun op bv
    combinations = [ (a,b, op) | (a,b) <- magicNumbers, op <- [minBound .. maxBound] ]


queryBenchmark :: String -> ((Bool -> Bool) -> BitVector -> Bool) -> Benchmark
queryBenchmark label mapFun = bgroup label $ generateBenchmark <$> combinations
  where
    generateBenchmark (intLabel, intValue, lOp) = bench message $ nf id target
      where
        message  = unwords ["query", getUnaryLogicalSymbol lOp, intLabel]
        !op      = getUnaryLogicalOperator lOp
        !bv      = bvGen intValue
        target   = mapFun op bv
    combinations = [ (a,b, op) | (a,b) <- magicNumbers, op <- [minBound .. maxBound] ]


bvGen :: Integer -> BitVector
bvGen x = force $ fromNumber (logBase2Word x) x


logBase2Word :: Integer -> Word
logBase2Word = succ . succ . ceiling . logBase (2.0 :: Double) . fromIntegral . abs


magicNumbers :: [(String, Integer)]
magicNumbers =
    [ ("zero"  ,            0)
    , ("tiny"  ,   tinyNumber)
    , ("small" ,  smallNumber)
    , ("medium", mediumNumber)
    , ("large" ,  largeNumber)
    , ("huge"  ,   hugeNumber)
    ]
