import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._
import stainless.lang._

import Environment._

trait UnknownInterface extends ContractInterface {
  def doSomething(): Unit
}

trait FieldMutated extends Contract {
  val target: Address
  var testField: Uint256

  @solidityPublic
  final def foo() = {
    val oldTestField = testField
    unsafeCast[UnknownInterface](target).doSomething()
    assert(oldTestField == testField)
  }
}
