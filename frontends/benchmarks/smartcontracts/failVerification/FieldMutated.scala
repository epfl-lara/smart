import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._
import stainless.lang._

trait FieldMutated extends Contract {
    var testField:Uint256

    final def invariant() = true

    final def foo() = {
        val oldTestField = testField
        havoc()
        // Shouldn't be valid
        assert(oldTestField == testField)
    }
}