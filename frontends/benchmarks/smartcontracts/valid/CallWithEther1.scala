import stainless.smartcontracts._
import stainless.smartcontracts.Environment._
import stainless.annotation._

trait CallWithEther1 extends Contract {
  @addressOfContract("CallWithEther1")
  val other: Address

  @solidityPublic
  def foo() = {
    dynRequire(
      this.addr.balance == Uint256("50") &&
      other.balance == Uint256("0")
    )

    pay(contractAt(other).asInstanceOf[CallWithEther1].bar, Uint256("50"))

    assert(
      other.balance == Uint256("50") &&
      this.addr.balance == Uint256("0")
    )
  }

  @solidityPayable
  @solidityPublic
  final def bar() = {

  }
}