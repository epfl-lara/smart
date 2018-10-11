import stainless.smartcontracts._
import stainless.lang._

import SafeMath._

object Util {
  def max(i: Uint256, j: Uint256) = {
    if (i > j) i
    else j
  }
  
  def subSwap(a: Uint256, b: Uint256, c: Uint256) = {
    a - b + add(b,c) == a + c
  } holds
}
