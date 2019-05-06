import stainless.smartcontracts._
import stainless.annotation._

trait FalseInvariant extends Contract {
  var x: BigInt

  final def increment() = {
    x = x + 1
  }

  @ghost
  final def invariant(): Boolean = false
}
