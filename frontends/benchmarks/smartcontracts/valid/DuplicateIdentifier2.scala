import stainless.smartcontracts._
import stainless.annotation._

object DuplicateIdentifier2 {
    case class DuplicateIdentifier2 (
        val i: Uint256
    ) extends Contract {
        @solidityPure
        def foo() = {
            val i:Boolean = true
            i
        }
    }
}