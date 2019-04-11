import stainless.smartcontracts._
import stainless.lang._

trait MappingType1 extends Contract {
  var m: MutableMap[Address, Uint256]

  def foo() = {
    m
  }
}
