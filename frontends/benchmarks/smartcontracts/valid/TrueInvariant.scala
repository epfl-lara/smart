import stainless.smartcontracts._

trait TrueInvariant extends Contract {
  var x: BigInt

  def increment() = {
    x = x + 1
  }

  final def invariant: Boolean = false
}
