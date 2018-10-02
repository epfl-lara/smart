import stainless.smartcontracts._

trait MappingType2 extends Contract {
  val m: Mapping[Address, Mapping[Address, Uint256]]
  
  def foo() = {
    m(Address(1))(Address(0)) = Uint256("50")
  }
}
