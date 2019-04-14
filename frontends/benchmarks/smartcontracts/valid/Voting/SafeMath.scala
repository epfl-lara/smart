import stainless.smartcontracts._
import stainless.annotation._

@solidityLibrary("SafeMath")
object SafeMath {
  @solidityPure
  @solidityPublic
  def add(a: Uint256, b: Uint256) = {
    val c = a + b
    dynRequire(c >= a)
    c
  }

  @solidityPure
  @solidityPublic
  def sub(a: Uint256, b: Uint256) = {
    dynRequire(b <= a)
    a - b
  }

  @solidityPure
  @solidityPublic
  def mul(a: Uint256, b: Uint256) = {
    val c = a * b
    dynRequire(a == Uint256.ZERO || c / a == b)
    c
  }

  @solidityPure
  @solidityPublic
  def div(a: Uint256, b: Uint256) = {
    dynRequire(b > Uint256.ZERO)
    a / b
  }
}
