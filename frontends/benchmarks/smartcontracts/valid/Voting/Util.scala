import stainless.smartcontracts._

object Util {
  def max(i: Uint256, j: Uint256) = {
    if (i > j) i
    else j
  }
  
  def subSwap(a: Uint256, b: Uint256, c: Uint256, d: Uint256) = {
    require(c - b == d)
    a - b + c == a + d
  } ensuring(res => res)
}