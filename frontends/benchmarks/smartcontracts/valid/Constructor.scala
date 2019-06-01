import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._

trait CC extends Contract {
  var a: Uint256
  var b: Uint256

  @solidityPublic
  final def constructor(_c: Uint256, _d: Uint256) = {
    a = _c;
    b = _c + _d;
  }
}
