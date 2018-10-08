import stainless.smartcontracts._
import stainless.annotation._

trait DynRequire1 extends Contract {
  @solidityView
  def foo() = {
    // Solidity require
    dynRequire(address(this).balance == Uint256("42"))

    // Stainless assertion
    assert(address(this).balance == Uint256("42"))
  }
}
