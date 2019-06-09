import stainless.smartcontracts._
import stainless.lang._
import stainless.collection._
import stainless.annotation._

trait VRB extends ContractInterface {
  def doSomething(): Unit
}

trait VRA extends Contract {
  var userBalance: Uint256
  var contractBalance: Uint256
  var totalCoins: Uint256

  @addressOfContract("VRB")
  val target: Address

  @ghost @inline
  final def invariant() = userBalance + contractBalance == totalCoins

  @solidityPublic
  final def constructor(_totalCoins: Uint256) = {
    // We temporarily use assume here but we must use something
    // that will be compiled so that this fails at runtime if invalid
    ghost(dynRequire(
      Environment.contractAt(target).isInstanceOf[VRB]
    ))

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

    Environment.contractAt(target).asInstanceOf[VRB].doSomething
  }
}
