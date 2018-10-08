import stainless.smartcontracts._
import stainless.annotation._

trait CallWithEther1 extends Contract {

  val other: CallWithEther1

  def foo() = {
    require(
      address(this).balance == Uint256("50") &&
      address(other).balance == Uint256("0")
    )

    pay(other.bar(), Uint256("50"))
  } ensuring { _ =>
    address(other).balance == Uint256("50") &&
    address(this).balance == Uint256.ZERO
  }

  @solidityPayable
  def bar() = {
    
  }
}
