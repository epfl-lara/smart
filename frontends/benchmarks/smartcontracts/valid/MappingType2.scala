import stainless.smartcontracts._
import stainless.lang._

trait MappingType2 extends Contract {
  val m: MutableMap[Address, MutableMap[Address, Uint256]]

  def foo() = {
    m(Address(1))(Address(0)) = Uint256("50")
  }
}
