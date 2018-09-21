import stainless.smartcontracts._

object DuplicateIdentifier1 {
    case class DuplicateIdentifier1() extends Contract {
        def foo(i: Uint256) = {
            val i: Uint256 = Uint256.ZERO
        }
    }
}