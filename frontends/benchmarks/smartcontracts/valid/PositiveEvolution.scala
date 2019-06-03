// FIXME : Evolution invariant is not implemented at the moment

/*import stainless.smartcontracts._
import stainless.annotation._

trait PositiveEvolution extends Contract {
  var x: BigInt

  @solidityPublic
  final def constructor() = {
    x = 2
  }

  @solidityPublic
  final def increment() = {
    x = x + 1
  }

  @ghost
  final def invariant: Boolean = x >= 0
  @ghost
  final def evolution(old: PositiveEvolution): Boolean = x >= old.x
  
}
*/
