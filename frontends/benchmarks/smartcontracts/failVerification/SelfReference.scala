import stainless.smartcontracts._
import stainless.annotation._
import stainless.lang._

import Environment._

trait SRDA extends Contract {
  var x: Uint256
  val other: Address

  @solidityPublic
  final def foo() = {
    val old = x
    unsafeCast[SRDA](other).increment()
    assert(old == x)
  }

  @solidityPublic
  final def increment() = x = x + Uint256.ONE
}
