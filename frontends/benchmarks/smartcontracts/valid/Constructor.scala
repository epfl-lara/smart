import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._

object Constructor {
  case class C(
    var a: Uint256,
	  var b: Uint256
  ) extends Contract { 
    def constructor(_c: Uint256, _d: Uint256) = {
      a = _c;
      b = _c + _d;
    }
  }
}
