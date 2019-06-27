import stainless.annotation._
import stainless.smartcontracts._

import Environment._

trait A extends Contract

trait B extends Contract {
  var x: BigInt

  final def constructor(_x: BigInt) = {
    dynAssert(_x >= 0)
    x = _x
  }

  @ghost
  final def invariant() = {
    x >= 0
  }
}
