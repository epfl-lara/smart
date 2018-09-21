import stainless.smartcontracts._
import stainless.annotation._

object SmartContract {
  case class Target() extends Contract {
    @payable
    def receiveMoney() = { }
  }

  case class Source(
    val targetContract:Target
  ) extends Contract {
    def send() = {
      require (
        this.addr.balance >= Uint256("50") &&
        targetContract.addr.balance == Uint256("0")
      )

      pay(targetContract.receiveMoney, Uint256("20"))
      assert(targetContract.addr.balance >= Uint256("20"))
    }
  }
}
