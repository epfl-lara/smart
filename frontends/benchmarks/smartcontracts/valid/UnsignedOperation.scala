import stainless.smartcontracts._
import stainless.annotation._
import stainless.math.wrapping

trait UnsignedOperation extends Contract {
  @solidityPure
  @solidityPublic
  final def safe_add(x: Uint256, y: Uint256) = {
    dynRequire(wrapping { x + y } >= x)
    x + y
  }

  @solidityPure
  @solidityPublic
  final def safe_minus(x: Uint256, y: Uint256) = {
    dynRequire(wrapping { x - y } <= x)
    x - y
  }

  @solidityPure
  @solidityPublic
  final def safe_times(x: Uint256, y: Uint256) = {
    dynRequire(
      x == Uint256.ZERO ||
      y == wrapping { x * y } / x
    )
    x * y
  }

  @solidityPure
  @solidityPublic
  final def safe_div(x: Uint256, y: Uint256) = {
    dynRequire(y > Uint256.ZERO)
    x / y
  }
}
