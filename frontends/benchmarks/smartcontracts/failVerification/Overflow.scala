import stainless.smartcontracts._
import stainless.annotation._

@wrapping // disable --strict-arithmetic checks inside the trait
object Test {
  @solidityPublic
  final def f(a: Uint256, b: Uint256) = {
    assert(a + b >= a)
  }
}
