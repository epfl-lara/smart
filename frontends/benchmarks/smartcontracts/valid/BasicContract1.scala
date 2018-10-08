import stainless.smartcontracts._
import stainless.annotation._

trait BasicContract1 extends Contract {
  val other: Address

  @solidityView
  def foo = {
    other
  }
}
