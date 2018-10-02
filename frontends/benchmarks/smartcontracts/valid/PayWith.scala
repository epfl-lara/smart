import stainless.smartcontracts._
import stainless.annotation._

trait Target extends Contract {
  @payable
  def receiveMoney() = { }
}

trait Source extends Contract {
  val targetContract: Target

  def send() = {
    require (
      this.addr.balance >= Uint256("50") &&
      targetContract.addr.balance == Uint256("0")
    )

    pay(targetContract.receiveMoney, Uint256("20"))
    assert(targetContract.addr.balance >= Uint256("20"))
  }
}
