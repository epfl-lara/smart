import stainless.smartcontracts._
import stainless.lang._
import stainless.collection._
import stainless.annotation._

import Environment._

trait RB2 extends ContractInterface {
  def doSomething(): Unit
}

trait RA2 extends Contract {
  var userBalance: Uint256
  var contractBalance: Uint256
  var totalCoins: Uint256

  val target: RB2

  @solidityPublic
  final def constructor(_totalCoins: Uint256) = {
    userBalance = Uint256.ZERO
    contractBalance = _totalCoins
    totalCoins = _totalCoins
  }

  @ghost
  final def invariant() = userBalance + contractBalance == totalCoins

  @solidityPublic
  final def withdrawBalance() = {
    val amount = userBalance

    // Changing the state here would be ok
    // totalCoins = totalCoins - amount
    // userBalance = Uint256.ZERO

    target.doSomething()

    // Shouldn't work to change the state here
    totalCoins = totalCoins - amount
    userBalance = Uint256.ZERO
  }
}
