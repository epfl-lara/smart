import stainless.smartcontracts._

trait MappingType4 extends Contract {
  val m: MutableMap[Address, MutableMap[Address, Uint256]]

  def foo() = {
    m(Address(0)) = MutableMap.withDefaultValue(Uint256.ZERO)
  }
}
