import stainless.smartcontracts._

trait MappingType5 extends Contract {
  var m: Mapping[Address, Uint256]

  def foo() = {
    m = Mapping.constant(Uint256.ZERO)
  }
}
