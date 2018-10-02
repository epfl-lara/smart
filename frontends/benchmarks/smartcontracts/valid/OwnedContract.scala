import stainless.smartcontracts._
import stainless.annotation._
import stainless.lang._

trait OwnedContract extends Contract {
  require(owner != addr)
  var owner: Address

  @payable
  def sendBalance() = {
    if(Msg.sender == owner) {
      val balance = address(this).balance
      owner.transfer(balance)
    }
  } ensuring { _ =>
    (Msg.sender == owner ==> (address(this).balance == Uint256.ZERO)) &&
    (Msg.sender != owner ==> (address(this).balance == address(old(this)).balance))
  }
}
