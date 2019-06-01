import stainless.smartcontracts._
import stainless.annotation._
import stainless.lang._

trait OwnedContract extends Contract {
  var owner: Address

  @solidityPayable
  @solidityPublic
  def sendBalance() = {
    dynRequire(
      !(addr == Msg.sender) &&
      addr.balance == Uint256("20")
    )

    if(Msg.sender == owner) {
      owner.transfer(addr.balance)
    }
  } ensuring { _ =>
    ((Msg.sender == owner) ==> (addr.balance == Uint256.ZERO)) &&
    (!(Msg.sender == owner) ==> (addr.balance == Uint256("20")))
  }
}
