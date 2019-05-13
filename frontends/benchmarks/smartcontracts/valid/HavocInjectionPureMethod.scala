import stainless.smartcontracts._
import stainless.annotation._

trait HIPM2 extends ContractInterface {
  @extern @pure
  def balanceOf():Uint256 = ???
}

trait HIPM1 extends Contract {
  @addressOfContract("HIPM2")
  val other:Address

  val x: Uint256

  @ghost
  final def invariant() = x == Uint256.ONE

  @solidityPublic
  final def foo() = {
    Environment.contractAt(other).asInstanceOf[HIPM2].balanceOf()
    assert(x == Uint256.ONE)
  }
}
