import stainless.smartcontracts._
import stainless.annotation._

trait TrueInvariant extends Contract {
  var x: BigInt

  @solidityPublic
  def increment() = {
    x = x + 1
  }

  @ghost
  final def invariant: Boolean = false
}
