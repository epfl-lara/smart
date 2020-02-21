import stainless.smartcontracts._
import stainless.annotation._
import stainless.lang._

import Environment._

@wrapping // disable --strict-arithmetic checks inside the trait
trait SRDA extends Contract {
  var x: Uint256
  val other: SRDA

  @solidityPublic
  final def foo() = {
    val old = x
    other.increment()
    assert(old == x)
  }

  @solidityPublic
  final def increment() = x = x + Uint256.ONE
}
