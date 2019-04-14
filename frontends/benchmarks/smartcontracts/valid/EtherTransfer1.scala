import stainless.smartcontracts._
import stainless.annotation._

trait EtherTransfer1 extends Contract {

  val other: PayableAddress

  @solidityPublic
  def foo(): Unit = {
      require(addr.balance == Uint256("50") &&
              other.balance == Uint256("0"))

      other.transfer(Uint256("50"))
  } ensuring { _ =>
      other.balance == Uint256("50") &&
      addr.balance == Uint256.ZERO
  }

}