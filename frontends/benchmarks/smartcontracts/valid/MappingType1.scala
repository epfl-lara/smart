import stainless.smartcontracts._

trait MappingType1 extends Contract {
  val m: Mapping[Address, Uint256]

  def foo() = {
    m(Address(0)) = Uint256("50")
  }
}
