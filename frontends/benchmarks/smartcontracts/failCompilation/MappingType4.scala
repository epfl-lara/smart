import stainless.smartcontracts._

trait MappingType4 extends Contract {
  val m: Mapping[Address, Mapping[Address, Uint256]]

  def foo() = {
    m(Address(0)) = Mapping.constant(Uint256.ZERO)
  }
}
