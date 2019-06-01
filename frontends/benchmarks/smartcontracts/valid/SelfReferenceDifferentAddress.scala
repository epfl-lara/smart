import stainless.smartcontracts._
import stainless.annotation._
import stainless.lang._

trait SRDA extends Contract {
  var x: Uint256

  @addressOfContract("SRDA")
  var other: Address

  @ghost
  final def invariant() = other != addr

  @solidityPublic
  final def foo() = {
    val old = x
    Environment.contractAt(other).asInstanceOf[SRDA].increment()
    assert(old == x)
  }

  @solidityPublic
  final def constructor(_other: Address) = {
    // We temporarily use assume here but we must use something
    // that will be compiled so that this fails at runtime if invalid
    ghost(assume(
      Environment.contractAt(other).isInstanceOf[SRDA] &&
      Environment.contractAt(other).asInstanceOf[SRDA].addr == other
    ))

    dynRequire(_other != addr)
    other = _other
  }

  @solidityPublic
  final def increment() = x = x + Uint256.ONE
}
