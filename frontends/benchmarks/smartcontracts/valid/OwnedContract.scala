import stainless.smartcontracts._
import stainless.annotation._
import stainless.lang._

trait OwnedContract extends Contract {
  var owner: Address

  @solidityPayable
  def sendBalance() = {
    require (owner != addr)
    if(Msg.sender == owner) {
      owner.transfer(addr.balance)
    }
  } ensuring { _ =>
    owner != addr &&
    (Msg.sender == owner ==> (addr.balance == Uint256.ZERO)) &&
    (Msg.sender != owner ==> (addr.balance == old(this).addr.balance))
  }
}
