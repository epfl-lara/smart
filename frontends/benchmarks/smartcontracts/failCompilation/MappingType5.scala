import stainless.smartcontracts._

trait MappingType5 extends Contract {
  var m: MutableMap[Address, Uint256]

  def foo() = {
    m = MutableMap.withDefaultValue(Uint256.ZERO)
  }
}
