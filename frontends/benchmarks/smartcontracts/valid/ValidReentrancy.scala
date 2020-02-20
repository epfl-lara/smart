import stainless.smartcontracts._
import stainless.lang._
import stainless.collection._
import stainless.annotation._

import Environment._

trait VRB extends ContractInterface {
  def doSomething(): Unit
}

@wrapping // disable --strict-arithmetic checks inside the trait
trait VRA extends Contract {
  var userBalance: Uint256
  var contractBalance: Uint256
  var totalCoins: Uint256

  val target: VRB

  @ghost @inline
  final def invariant() = userBalance + contractBalance == totalCoins

  @solidityPublic
  final def constructor(_totalCoins: Uint256) = {
    totalCoins = _totalCoins
    contractBalance = _totalCoins
    userBalance = Uint256.ZERO
  }

  @solidityPublic
  final def withdrawBalance() = {
    val amount = userBalance

    assert(userBalance + contractBalance == totalCoins)

    totalCoins = totalCoins - amount
    userBalance = Uint256.ZERO

    assert(userBalance + contractBalance == totalCoins)

    target.doSomething()
  }
}
