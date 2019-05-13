import stainless.smartcontracts._
import stainless.annotation._

trait HIPM2 extends Contract {
  @extern @pure
  @solidityView
  def balanceOf():Uint256 = ???
}

trait HIPM1 extends Contract {
  @addressOfContract("HIPM2")
  val other:Address

  val x: Uint256

  @ghost
  def invariant() = x == Uint256.ONE

  @solidityPublic
  def foo() = {
    Environment.contractAt(other).asInstanceOf[HIPM2].balanceOf()
    assert(x == Uint256.ONE)
  }
}
