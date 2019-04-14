import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._

trait MappingType2 extends Contract {
  val m: MutableMap[Address, MutableMap[Address, Uint256]]

  @solidityPublic
  def foo() = {
    m(Address(1))(Address(0)) = Uint256("50")
  }
}
