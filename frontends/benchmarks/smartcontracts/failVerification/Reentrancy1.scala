import stainless.smartcontracts._
import stainless.lang._
import stainless.collection._
import stainless.annotation._

trait B extends Contract {
  def doSomething():Unit = ???
}

trait A extends Contract {
  var userBalance: Uint256
  var contractBalance: Uint256
  var totalCoins: Uint256

  val target:Address

  final def invariant() =
    // Used to simply verification
    userBalance <= Uint256("30") &&
    userBalance + contractBalance == totalCoins

  final def withdrawBalance() = {
    require(Environment.contractAt(target).isInstanceOf[B])

    val amount = userBalance
      
    //totalCoins = totalCoins - amount
    //userBalance = Uint256.ZERO

    Environment.contractAt(target).asInstanceOf[B].doSomething

    // Shouldn't work to change the state here
    totalCoins = totalCoins - amount
    userBalance = Uint256.ZERO
  }

  final def transfer(amount: Uint256) = {
    if(amount <= userBalance) {
      contractBalance = contractBalance + amount
      userBalance = userBalance - amount
    }
  }
}
