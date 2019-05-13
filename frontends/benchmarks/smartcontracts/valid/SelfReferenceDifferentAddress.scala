import stainless.smartcontracts._
import stainless.annotation._

trait SRDA extends Contract {
  var x: Uint256

  @addressOfContract("SRDA")
  val other: Address

  def foo() = {
    require(other != addr)

    val old = x
    Environment.contractAt(other).asInstanceOf[SRDA].increment()
    assert(old == x)
  }

  def increment() = x = x + Uint256.ONE
}