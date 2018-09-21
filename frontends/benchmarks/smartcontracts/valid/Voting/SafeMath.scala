import stainless.smartcontracts._
import stainless.annotation._

@solidityLibrary("SafeMath")
object SafeMath {
  @solidityPure
  def add(a: Uint256, b: Uint256) = {
    val c = a + b
    dynRequire(c >= a)
    c
  }

  @solidityPure
  def sub(a: Uint256, b: Uint256) = {
    dynRequire(b <= a) 
    a - b
  }

  @solidityPure
  def mul(a: Uint256, b: Uint256) = {
    val c = a * b
    dynRequire(a == Uint256.ZERO || c / a == b)
    c
  }

  @solidityPure
  def div(a: Uint256, b: Uint256) = {
    dynRequire(b > Uint256.ZERO)
    a / b
  }
}