### Unreleased Changes

  * None

### [v1.0.1][4]

  * Correcting Eq instance to test for value equality and not construction equality

  * Updated unit tests do not fail when the antecedent of logical implication cannot be satisfied


### [v1.0.0][3]

  * Added explicit recursion to monomorphic folds to improve time and space performance

  * Added the following instances:
    * `MonoAdjustable`
    * `MonoFoldableWithKey`
    * `MonoIndexable`
    * `MonoKeyed`
    * `MonoLookup`
    * `MonoTraversableWithKey`
    * `MonoZip`
    * `MonoZipWithKey`
    * `TextShow`

  * Updated the following functions to be constant time:
    * `omap`
    * `olength64`
    * `oall`
    * `oany`
    * `ofoldr1Ex`
    * `ofoldl1Ex`
    * `oelem`
    * `onotElem`
    * `ozipWith`


### [v0.1.2][2]

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
[3]: https://github.com/recursion-ninja/bv-little/tree/v1.0.0
[4]: https://github.com/recursion-ninja/bv-little/tree/v1.0.1
