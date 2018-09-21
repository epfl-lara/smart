import stainless.smartcontracts._
import stainless.annotation._

object GhostParameter {
  case class C() extends Contract {
    
    @solidityPure
    def f(@ghost a: Uint256) = {
    }

    @solidityPure
    def g() = f(Uint256.ZERO)
  }
}
