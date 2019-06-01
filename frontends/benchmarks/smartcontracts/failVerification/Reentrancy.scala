import stainless.smartcontracts._
import stainless.lang._
import stainless.collection._
import stainless.annotation._

trait RB extends Contract {
  def doSomething(): Unit
}

trait RA extends Contract {
  var userBalance: Uint256
  var contractBalance: Uint256
  var totalCoins: Uint256

  @addressOfContract("RB")
  val target: Address

  @solidityPublic
  final def constructor(_totalCoins: Uint256) = {
    // We temporarily use assume here but we must use something
    // that will be compiled so that this fails at runtime if invalid
    ghost(assume(
      Environment.contractAt(target).isInstanceOf[RB] &&
      Environment.contractAt(target).asInstanceOf[RB].addr == target
    ))

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

    Environment.contractAt(target).asInstanceOf[RB].doSomething

    // Shouldn't work to change the state here
    totalCoins = totalCoins - amount
    userBalance = Uint256.ZERO
  }
}
