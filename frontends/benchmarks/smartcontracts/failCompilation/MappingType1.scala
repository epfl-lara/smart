import stainless.smartcontracts._

trait MappingType1 extends Contract {
  var m: Mapping[Address, Uint256]

  def foo() = {
    m
  }
}
