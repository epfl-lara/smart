import stainless.smartcontracts._
import stainless.lang._
import stainless.collection._
import stainless.annotation._

trait RB extends Contract {
  @solidityPublic
  def doSomething(): Unit
}

trait RA extends Contract {
  var userBalance: Uint256
  var contractBalance: Uint256
  var totalCoins: Uint256

  @addressOfContract("RB")
  val target: Address

  @ghost
  final def invariant() = userBalance + contractBalance == totalCoins

  @solidityPublic
  final def withdrawBalance() = {
    val amount = userBalance

    // Changing the state here would be ok
    // totalCoins = totalCoins - amount
    // userBalance = Uint256.ZERO

    Environment.contractAt(target).asInstanceOf[RB].doSomething

    // Shouldn't work to change the state here
    totalCoins = totalCoins - amount
    userBalance = Uint256.ZERO
  }
}
