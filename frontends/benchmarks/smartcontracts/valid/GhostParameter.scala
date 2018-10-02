import stainless.smartcontracts._
import stainless.annotation._

trait GhostParameter extends Contract {

  @solidityPure
  def f(@ghost a: Uint256) = {
  }

  @solidityPure
  def g() = f(Uint256.ZERO)
  
}
