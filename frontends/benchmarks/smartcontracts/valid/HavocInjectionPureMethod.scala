import stainless.smartcontracts._
import stainless.annotation._
import stainless.lang._

trait HIPM2 extends ContractInterface {
  @extern @pure
  def balanceOf():Uint256 = ???
}

trait HIPM1 extends Contract {
  @addressOfContract("HIPM2")
  val other:Address

  var x: Uint256

  @solidityPublic
  final def constructor() = {
    // We temporarily use assume here but we must use something
    // that will be compiled so that this fails at runtime if invalid
    ghost(assume(
      Environment.contractAt(other).isInstanceOf[HIPM2] &&
      Environment.contractAt(other).asInstanceOf[HIPM2].addr == other
    ))

    x = Uint256.ONE
  }

  @ghost
  final def invariant() = x == Uint256.ONE

  @solidityPublic
  final def foo() = {
    Environment.contractAt(other).asInstanceOf[HIPM2].balanceOf()
    assert(x == Uint256.ONE)
  }
}
