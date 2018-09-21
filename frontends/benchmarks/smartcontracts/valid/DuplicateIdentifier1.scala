import stainless.smartcontracts._
import stainless.annotation._

object DuplicateIdentifier1 {
    case class DuplicateIdentifier1(
        val i: Uint256
    ) extends Contract {
        @solidityPure
        def foo(@ghost i: Uint256) = true
    }
}