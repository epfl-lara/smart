import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._
import stainless.lang._

trait OCMB extends Contract {
    var testField1: Uint256
    val testField2: Uint256
}

trait OCMA extends Contract {
    val target:Address

    final def invariant() = true

    final def foo() = {
        require(
            Environment.contractAt(target).isInstanceOf[B]
        )

        val oldTestField1 = Environment.contractAt[OCMB](target).testField1
        val oldTestField2 = Environment.contractAt[OCMB](target).testField2
        havoc()
        assert(oldTestField1 == Environment.contractAt[OCMB](target).testField1)
        assert(oldTestField2 == Environment.contractAt[OCMB](target).testField2)
    }

}