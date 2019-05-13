import stainless.smartcontracts._
import stainless.annotation._

object Test {
  @solidityPublic
  def f(a: Uint256, b: Uint256) = {
    assert(a + b >= a)
  }
}
