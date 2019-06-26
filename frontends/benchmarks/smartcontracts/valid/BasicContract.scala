import stainless.smartcontracts._
import stainless.annotation._

trait BasicContract extends Contract {
  val other: Address

  @solidityView
  @solidityPublic
  final def foo(): Address = {
    other
  }
}
