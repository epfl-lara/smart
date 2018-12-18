import stainless.smartcontracts._

trait PositiveEvolution extends Contract {
  var x: BigInt

  final def constructor() = {
    x = 2
  }

  final def increment() = {
    x = x + 1
  }

  final def invariant: Boolean = x >= 0
  final def evolution(old: PositiveEvolution): Boolean = x >= old.x
}
