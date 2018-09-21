import stainless.smartcontracts._
import stainless.annotation._

@solidityLibrary("BasicLibrary1")
object BasicLibrary1 {
    @solidityPure
    def foo() = true

    @solidityPure
    def bar() = false
}