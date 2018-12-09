import stainless.smartcontracts._
import stainless.annotation._

trait CallWithEther1 extends Contract {

  val other: CallWithEther1

  def foo() = {
    require(
      this.addr.balance == Uint256("50") &&
      other.addr.balance == Uint256("0")
    )

    pay(other.bar(), Uint256("50"))
  } ensuring { _ =>
    other.addr.balance == Uint256("50") &&
    this.addr.balance == Uint256("0")
  }

  @solidityPayable
  final def bar() = {

  }
}
