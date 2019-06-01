import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._
import stainless.lang._

trait UnknownInterface extends Contract {
  def doSomething()
}

trait FieldMutated extends Contract {
  val target: Address
  var testField:Uint256

  @solidityPublic
  final def constructor() = {
    // We temporarily use assume here but we must use something
    // that will be compiled so that this fails at runtime if invalid
    ghost(assume(
      Environment.contractAt(target).isInstanceOf[UnknownInterface] &&
      Environment.contractAt(target).asInstanceOf[UnknownInterface].addr == target
    ))
  }

  @ghost
  final def invariant() = Environment.contractAt(target).isInstanceOf[UnknownInterface]

  @solidityPublic
  final def foo() = {
      val oldTestField = testField
      Environment.contractAt(target).asInstanceOf[UnknownInterface].doSomething()
      assert(oldTestField == testField)
  }
}