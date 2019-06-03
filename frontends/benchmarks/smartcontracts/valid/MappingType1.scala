import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._

trait MappingType1 extends Contract {
  var m: MutableMap[Address, Uint256]

  @solidityPublic
  def foo() = {
    m(Address(0)) = Uint256("50")
  }
}
