import stainless.smartcontracts._
import stainless.annotation._

trait PrivateMethodValid extends Contract {
  var balance: Uint256

  @ghost
  final def invariant() = balance >= Uint256("20")

  @solidityPublic
  final def constructor() = {
      balance = Uint256("20")
  }

  @solidityPublic
  final def setBalance(newBalance: Uint256) = {
    balance = Uint256.ZERO
    balance = clamp(newBalance)
  }

  final def clamp(x: Uint256) = {
    if(x >= Uint256("20")) x
    else Uint256("20")
  }
}