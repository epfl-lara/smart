import stainless.smartcontracts._
import stainless.annotation._

trait BasicContract1 extends Contract {
  val other: Address

  @view
  def foo = {
    other
  }
}
