// FIXME: Known external calls optimization

import stainless.smartcontracts._
import stainless.smartcontracts.Environment._
import stainless.annotation._

trait CallWithEther1 extends Contract {
  @addressOfContract("CallWithEther1")
  val other: Address

  @solidityPublic
  final def foo() = {
    require(
      this.addr.balance == Uint256("50") &&
      other.balance == Uint256("0")
    )

    contractAt(other).asInstanceOf[CallWithEther1].bar()
    //pay(contractAt(other).asInstanceOf[CallWithEther1].bar, Uint256("50"))
  } ensuring { _ =>
    other.balance == Uint256("50") &&
    this.addr.balance == Uint256("0")
  }

  @solidityPayable
  @solidityPublic
  final def bar() = {

  }
}
