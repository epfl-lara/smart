import stainless.smartcontracts._
import stainless.lang._
import stainless.collection._
import stainless.annotation._

trait VRB extends Contract {
  def doSomething(): Unit
}

trait VRA extends Contract {
  var userBalance: Uint256
  var contractBalance: Uint256
  var totalCoins: Uint256

  @addressOfContract("VRB")
  val target: Address

  final def invariant() = userBalance + contractBalance == totalCoins

  final def withdrawBalance() = {
    val amount = userBalance

    assert(userBalance + contractBalance == totalCoins)

    totalCoins = totalCoins - amount
    userBalance = Uint256.ZERO

    assert(userBalance + contractBalance == totalCoins)

    Environment.contractAt(target).asInstanceOf[VRB].doSomething
  }
}
