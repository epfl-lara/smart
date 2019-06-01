import stainless.smartcontracts._
import stainless.annotation._
import stainless.lang._

trait SRDA extends Contract {
  var x: Uint256

  @addressOfContract("SRDA")
  val other: Address

  @solidityPublic
  final def constructor() = {
    // We temporarily use assume here but we must use something
    // that will be compiled so that this fails at runtime if invalid
    ghost(assume(
      Environment.contractAt(target).isInstanceOf[SRDA] &&
      Environment.contractAt(target).asInstanceOf[SRDA].addr == target
    ))
  }

  @solidityPublic
  def foo() = {
    val old = x
    Environment.contractAt(other).asInstanceOf[SRDA].increment()
    assert(old == x)
  }

  @solidityPublic
  def increment() = x = x + Uint256.ONE
}