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

    @library
    final def invariant() = Environment.contractAt(target).isInstanceOf[UnknownInterface]

    final def foo() = {
        val oldTestField = testField
        Environment.contractAt(target).asInstanceOf[UnknownInterface].doSomething()
        assert(oldTestField == testField)
    }
}