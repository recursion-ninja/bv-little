## Efficient little-endian bit vector Haskell library

[![Build Status](https://travis-ci.org/recursion-ninja/bv-little.svg?branch=master)](https://travis-ci.org/recursion-ninja/bv-little)
[![Workflow Status](https://github.com/recursion-ninja/bv-little/workflows/build/badge.svg?branch=master)](https://github.com/recursion-ninja/bv-little/actions)
[![Coverage Status](https://coveralls.io/repos/github/recursion-ninja/bv-little/badge.svg?branch=master)](https://coveralls.io/github/recursion-ninja/bv-little?branch=master)
[![License FreeBSD](https://img.shields.io/badge/license-FreeBSD-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)

[![Hackage](https://img.shields.io/hackage/v/bv-little.svg?style=flat&color=brightgreen)](https://hackage.haskell.org/package/bv-little)
[![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/bv-little/badge)](https://matrix.hackage.haskell.org/package/bv-little)

[![Stackage LTS](http://stackage.org/package/bv-little/badge/lts)](http://stackage.org/lts/package/bv-little)
[![Stackage Nightly](http://stackage.org/package/bv-little/badge/nightly?style=flat&color=green)](http://stackage.org/nightly/package/bv-little)


This package contains an efficient implementation of *little-endian, immutable* bit vectors. It implements most applicable typeclasses and also conversions to and from signed or unsigned numbers.

For an implementation of *big-endian, immutable* bit vectors, use the [`bv`](https://hackage.haskell.org/package/bv) package.

For an implementation of *little-endian, mutable* bit vectors, use the [`bitvec`](https://hackage.haskell.org/package/bitvec) package.

#### Tests

The test suite ensures that all typeclass instances are "lawful" and that data-structure–specific functionality is well defined.

The `TestSuite.hs` file contains the specification. It can be run by invoking any of the following commands:

  * `cabal new-test`

  * `cabal test`

  * `stack test`

#### Benchmarks

The benchmarks provide an empyrical check for the asymptotic complexity of data structure operations and also provide easy metrics for detecting performance regressions.

The `Benchmaks.hs` file contains these metrics. It can be run by invoking any of the following commands:

  * `cabal new-bench`

  * `cabal bench`

  * `stack bench`
