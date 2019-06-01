import stainless.smartcontracts._
import stainless.annotation._
import stainless.lang.StaticChecks._
import stainless.lang.ghost

trait HavocInvariant1 extends Contract {
  var balance: Uint256
  var empty: Boolean

  @ghost
  final def invariant():Boolean = {
    ((balance == Uint256.ZERO) && empty) ||
    ((balance > Uint256.ZERO) && !empty)
  }

  @solidityPublic
  final def withdrawBalance() = {
    dynRequire(balance >= Uint256.ONE)
    balance = Uint256.ZERO
    empty = true

    assert( invariant() )
    Msg.sender.transfer(Uint256.ONE)
    assert( invariant() )
  }

  @solidityPublic
  final def constructor(_balance: Uint256) = {
    balance = _balance
    empty = !(balance > Uint256.ZERO)
  }
}