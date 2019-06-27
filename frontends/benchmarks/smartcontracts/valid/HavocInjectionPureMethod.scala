import stainless.smartcontracts._
import stainless.annotation._
import stainless.lang._

import Environment._

trait HIPM2 extends ContractInterface {
  @extern @pure
  def balanceOf():Uint256 = ???
}

trait HIPM1 extends Contract {
  val other: HIPM2

  var x: Uint256

  @solidityPublic
  final def constructor() = {
    x = Uint256.ONE
  }

  @ghost
  final def invariant() = x == Uint256.ONE

  @solidityPublic
  final def foo() = {
    other.balanceOf()
    assert(x == Uint256.ONE)
  }
}
