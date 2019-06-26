import stainless.smartcontracts._
import stainless.annotation._

object Test {
  @solidityPublic
  final def f(a: Uint256, b: Uint256) = {
    assert(a + b >= a)
  }
}
