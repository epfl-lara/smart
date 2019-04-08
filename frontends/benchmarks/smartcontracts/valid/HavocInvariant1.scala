import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._
import stainless.lang._

trait HavocInvariant1 extends Contract {
    var balance: Uint256
    var empty: Boolean

    final def invariant():Boolean = {
        ((balance == Uint256.ZERO) && empty) ||
        ((balance > Uint256.ZERO) && !empty)
    }

    final def withdrawBalance() = {
        balance = Uint256.ZERO
        empty = true

        assert( invariant() )
        Msg.sender.transfer(Uint256.ONE)
        havoc()
        assert( invariant() )
    }

}