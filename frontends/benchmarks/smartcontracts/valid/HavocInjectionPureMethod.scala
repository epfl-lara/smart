import stainless.smartcontracts._
import stainless.annotation._
import stainless.lang._

import Environment._

trait HIPM2 extends ContractInterface {
  @extern @pure
  def balanceOf():Uint256 = ???
}

trait HIPM1 extends Contract {
  val other:Address

  var x: Uint256

  @solidityPublic
  final def constructor() = {
    x = Uint256.ONE
  }

  @ghost
  final def invariant() = x == Uint256.ONE

  @solidityPublic
  final def foo() = {
    unsafeCast[HIPM2](other).balanceOf()
    assert(x == Uint256.ONE)
  }
}
