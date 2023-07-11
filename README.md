## Efficient little-endian bit vector Haskell library

[![Build Status](https://travis-ci.org/recursion-ninja/bv-little.svg?branch=master)](https://travis-ci.org/recursion-ninja/bv-little)
[![Workflow Status](https://github.com/recursion-ninja/bv-little/workflows/build/badge.svg?branch=master)](https://github.com/recursion-ninja/bv-little/actions)
[![Coverage Status](https://coveralls.io/repos/github/recursion-ninja/bv-little/badge.svg?branch=master)](https://coveralls.io/github/recursion-ninja/bv-little?branch=master)
[![License FreeBSD](https://img.shields.io/badge/license-FreeBSD-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)

[![Hackage](https://img.shields.io/hackage/v/bv-little.svg?style=flat&color=brightgreen)](https://hackage.haskell.org/package/bv-little)
[![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/bv-little/badge)](https://matrix.hackage.haskell.org/package/bv-little)

[![Stackage LTS](http://stackage.org/package/bv-little/badge/lts)](http://stackage.org/lts/package/bv-little)
[![Stackage Nightly](http://stackage.org/package/bv-little/badge/nightly?style=flat&color=green)](http://stackage.org/nightly/package/bv-little)


This package contains an efficient implementation of *little-endian, immutable* bit vectors. It implements most applicable type-classes and also conversions to and from signed or unsigned numbers.

For an implementation of *big-endian, immutable* bit vectors, use the [`bv`](https://hackage.haskell.org/package/bv) package.

For an implementation of *little-endian, mutable* bit vectors, use the [`bitvec`](https://hackage.haskell.org/package/bitvec) package.


### Accessing type-class instances

This package utilizes the ["multiple sub-libraries"][0] feature of Cabal.
The intended usage is an "opt-in" dependency footprint for the `bv-little` package, as not all type-class instances which are defined are exposed by default.
Therefore package consumers can select which, if any, type-class instances outside of the [core libraries][1] they wish to have exposed and transitively depend on the associated package(s).

#### New type-class instance exposure procedure:

1. All `BitVector` instances of type-classes defined in `base` are exported by default from the `Data.BitVector.LittleEndian` module.

2. Each `BitVector` instance of a type-class defined in a package *other than* `base` is exposed through a specific sub-library dependency and a special exposing module.

To access an instance of a type-class defined *outside* `base`, add the requisite sub-library to your `build-depends` and `import`  the corresponding exposing module within your code-base.

| Sub-library Dependency                      | Exposing Module                               | Type-class Instance(s)       |
|:--------------------------------------------|:----------------------------------------------|:-----------------------------|
| `bv-little:instances-binary`                | `Data.BitVector.LittleEndian.Binary`          | <ul><li>`Binary`</li></ul>   |
| `bv-little:instances-mono-traversable`      | `Data.BitVector.LittleEndian.MonoTraversable` | <ul><li>`MonoFoldable`</li><li>`MonoFunctor`</li><li>`MonoTraversable`</li></ul> |
| `bv-little:instances-mono-traversable-keys` | `Data.BitVector.LittleEndian.MonoKeyed`       | <ul><li>`MonoAdjustable`</li><li>`MonoFoldableWithKey`</li><li>`MonoIndexable`</li><li>`MonoKeyed`</li><li>`MonoLookup`</li><li>`MonoTraversableWithKey`</li><li>`MonoZip`</li><li>`MonoZipWithKey`</li></ul> |
| `bv-little:instances-quickcheck`            | `Data.BitVector.LittleEndian.QuickCheck`      | <ul><li>`Arbitrary`</li><li>`CoArbitrary`</li></ul> |
| `bv-little:instances-text-show`             | `Data.BitVector.LittleEndian.TextShow`        | <ul><li>`TextShow`</li></ul> |


### Tests

The test suite ensures that all type-class instances are "lawful" and that data-structure–specific functionality is well defined.

The `TestSuite.hs` file contains the specification. It can be run by invoking any of the following commands:

  * `cabal test`

  * `stack test`


### Benchmarks

The benchmarks provide an empyrical check for the asymptotic complexity of data structure operations and also provide easy metrics for detecting performance regressions.

The `Benchmarks.hs` file contains these metrics. It can be run by invoking any of the following commands:

  * `cabal bench`

  * `stack bench`


[0]: https://cabal.readthedocs.io/en/3.4/cabal-package.html?highlight=visibility#sublibs
[1]: https://github.com/haskell/core-libraries-committee#core-libraries