import stainless.smartcontracts._
import stainless.lang._

trait MappingType1 extends Contract {
  val m: MutableMap[Address, Uint256]

  def foo() = {
    m(Address(0)) = Uint256("50")
  }
}
