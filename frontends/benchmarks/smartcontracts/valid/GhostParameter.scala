import stainless.smartcontracts._
import stainless.annotation._

trait GhostParameter extends Contract {

  @solidityPure
  @solidityPublic
  final def f(@ghost a: Uint256) = {
  }

  @solidityPure
  @solidityPublic
  final def g() = f(Uint256.ZERO)
}
