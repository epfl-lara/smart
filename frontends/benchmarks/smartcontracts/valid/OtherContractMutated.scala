import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._
import stainless.lang._

trait OCMB extends Contract {
  @solidityPublic
  var testField1: Uint256

  @solidityPublic
  val testField2: Uint256
}

trait OCMA extends Contract {
  val target:Address

  @ghost
  final def invariant() = true

  @solidityPublic
  @solidityView
  final def foo() = {
    require(
      Environment.contractAt(target).isInstanceOf[OCMB]
    )

    val oldTestField1 = Environment.contractAt(target).asInstanceOf[OCMB].testField1
    val oldTestField2 = Environment.contractAt(target).asInstanceOf[OCMB].testField2
    assert(oldTestField1 == Environment.contractAt(target).asInstanceOf[OCMB].testField1)
    assert(oldTestField2 == Environment.contractAt(target).asInstanceOf[OCMB].testField2)
  }

}
