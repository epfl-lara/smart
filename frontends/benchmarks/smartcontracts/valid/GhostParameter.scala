import stainless.smartcontracts._
import stainless.annotation._

trait GhostParameter extends Contract {

  @solidityPure
  @solidityPublic
  def f(@ghost a: Uint256) = {
  }

  @solidityPure
  @solidityPublic
  def g() = f(Uint256.ZERO)
}
