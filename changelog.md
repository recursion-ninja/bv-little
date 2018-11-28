### Unreleased Changes

  * Updated `length64`, `oall`, `oany`, `omap`, `maximumByEx`, & `minimumByEx`, oelem, onotElem to be constant time

  * Added explicit recursion to monomorphic folds to improve time and space performance


### [v0.1.2][1]

  * Updating to base bounds for GHC 8.6.1


### [v0.1.1][1]

  * Updated to well-typed internal representation

  * Corrected defect in rotate, rotateL, and rotateR

  * Improved performance by switching exponentiation with base 2 to bit shifting operations

  * Improved performance of clearBit by increasing strictness

  * Increased benchmark coverage

  * Increased test suite coverage


### [v0.1.0][0]

  * Created instances of applicable typeclass instances

  * Added numeric conversion functions

  * Added basic test suite

  * Added stub benchmark


[0]: https://github.com/recursion-ninja/bv-little/tree/v0.1.0
[1]: https://github.com/recursion-ninja/bv-little/tree/v0.1.1
[2]: https://github.com/recursion-ninja/bv-little/tree/v0.1.2
