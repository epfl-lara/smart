import stainless.smartcontracts._

object Test {
  def f(a: Uint256, b: Uint256) = {
    assert(a + b >= a)
  }
}
