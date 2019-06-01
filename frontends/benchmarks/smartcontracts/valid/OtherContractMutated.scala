import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._
import stainless.lang._

trait OCMB extends Contract {
  @solidityPublic
  var testField1: Uint256

  @solidityPublic
  var testField2: Uint256

  @ghost
  final def invariant() = testField2 == Uint256.ONE

  @solidityPublic
  final def constructor() = {
    testField2 = Uint256.ONE
  }

  @solidityPublic
  @solidityView
  final def doNothing() = {
    // Do Nothing
  }
}

trait OCMA extends Contract {
  @addressOfContract("OCMB")
  val target:Address

  @solidityPublic
  final def constructor() = {
    // We temporarily use assume here but we must use something
    // that will be compiled so that this fails at runtime if invalid
    ghost(assume(
      Environment.contractAt(target).isInstanceOf[OCMB] &&
      Environment.contractAt(target).asInstanceOf[OCMB].addr == target
    ))
  }

  @solidityPublic
  @solidityView
  final def foo() = {

    val oldTestField1 = Environment.contractAt(target).asInstanceOf[OCMB].testField1
    val oldTestField2 = Environment.contractAt(target).asInstanceOf[OCMB].testField2

    Environment.contractAt(target).asInstanceOf[OCMB].doNothing

    assert(oldTestField2 == Environment.contractAt(target).asInstanceOf[OCMB].testField2)
  }

}
