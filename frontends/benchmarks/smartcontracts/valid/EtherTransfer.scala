import stainless.smartcontracts._
import stainless.annotation._

import Environment._

trait EtherTransfer extends Contract {
  val other: PayableAddress

  @solidityPublic
  final def foo(): Unit = {
    dynRequire(
      addr.balance == Uint256("50") &&
      other.balance == Uint256("0"))

    other.transfer(Uint256("50"))
  } ensuring { _ =>
    other.balance == Uint256("50") &&
    addr.balance == Uint256.ZERO
  }
}
