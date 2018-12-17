import stainless.smartcontracts._

trait FalseInvariant extends Contract {
  var x: BigInt

  final def increment() = {
    x = x + 1
  }

  final def invariant: Boolean = false
}
