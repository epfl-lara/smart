import stainless.smartcontracts._
import stainless.lang._
import stainless.collection._
import stainless.annotation._

import Environment._

trait RB extends ContractInterface {
  def doSomething(): Unit
}

trait RA extends Contract {
  var userBalance: Uint256
  var contractBalance: Uint256
  var totalCoins: Uint256

  val target: Address

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

    unsafeCast[RB](target).doSomething()

    // Shouldn't work to change the state here
    totalCoins = totalCoins - amount
    userBalance = Uint256.ZERO
  }
}
