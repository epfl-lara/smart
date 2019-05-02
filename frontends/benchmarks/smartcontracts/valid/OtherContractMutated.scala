import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._
import stainless.lang._

trait OCMB extends Contract {
  @solidityPublic
  var testField1: Uint256

  @solidityPublic
  val testField2: Uint256

  final def invariant() = testField2 == Uint256.ONE

  final def doNothing() = {
    // Do Nothing
  }
}

trait OCMA extends Contract {
  val target:Address

  @ghost
  final def invariant() = Environment.contractAt(target).isInstanceOf[OCMB] &&
                          Environment.contractAt(target).asInstanceOf[OCMB].invariant()

  @solidityPublic
  @solidityView
  final def foo() = {

    val oldTestField1 = Environment.contractAt(target).asInstanceOf[OCMB].testField1
    val oldTestField2 = Environment.contractAt(target).asInstanceOf[OCMB].testField2

    Environment.contractAt(target).asInstanceOf[OCMB].doNothing

    assert(oldTestField2 == Environment.contractAt(target).asInstanceOf[OCMB].testField2)
  }

}
