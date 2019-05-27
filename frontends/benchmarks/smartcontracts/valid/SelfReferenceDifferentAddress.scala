import stainless.smartcontracts._
import stainless.annotation._

trait SRDA extends Contract {
  var x: Uint256

  @addressOfContract("SRDA")
  val other: Address

  @ghost
  final def invariant() = other != addr

  @solidityPublic
  final def foo() = {
    val old = x
    Environment.contractAt(other).asInstanceOf[SRDA].increment()
    assert(old == x)
  }

  @solidityPublic
  final def increment() = x = x + Uint256.ONE
}
