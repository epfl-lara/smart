import stainless.smartcontracts._

trait DuplicateIdentifier1 extends Contract {
  def foo(i: Uint256) = {
    val i: Uint256 = Uint256.ZERO
  }
}
