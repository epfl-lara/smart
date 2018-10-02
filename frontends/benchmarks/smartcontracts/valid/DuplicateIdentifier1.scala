import stainless.smartcontracts._
import stainless.annotation._

trait DuplicateIdentifier1 extends Contract {
  val i: Uint256

  @solidityPure
  def foo(@ghost i: Uint256) = true
}
