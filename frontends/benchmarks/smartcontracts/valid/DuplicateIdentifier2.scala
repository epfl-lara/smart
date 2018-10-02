import stainless.smartcontracts._
import stainless.annotation._

trait DuplicateIdentifier2 extends Contract {
  val i: Uint256

  @solidityPure
  def foo() = {
    val i: Boolean = true
    i
  }

}