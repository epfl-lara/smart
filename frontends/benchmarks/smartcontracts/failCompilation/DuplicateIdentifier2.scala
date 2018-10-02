import stainless.smartcontracts._

trait DuplicateIdentifier2 extends Contract {
  def foo() = {
    val i: Uint256 = Uint256.ZERO
    if (true) {
      val i: Uint256 = Uint256.ZERO
      true
    } else false
  }
}
