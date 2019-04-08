import stainless.smartcontracts._
import stainless.lang._
import stainless.collection._
import stainless.annotation._

trait A extends Contract {
    var userBalance: Uint256
    var contractBalance: Uint256

    var totalCoins: Uint256

    final def invariant() = userBalance + contractBalance == totalCoins

    final def withdrawBalance() = {
        val amount = userBalance
        
        /*totalCoins = totalCoins - amount
        userBalance = Uint256.ZERO*/

        assert(invariant())
        Msg.sender.transfer(amount)
        havoc()
        assert(invariant())

        // Shouldn't work to change the state here
        totalCoins = totalCoins - amount
        userBalance = Uint256.ZERO
    } ensuring { _ =>
        inv()
    }

    final def transfer(amount: Uint256) = {
        if(amount <= userBalance) {
            contractBalance = contractBalance + amount
            userBalance = userBalance - amount
        }
    }
}
