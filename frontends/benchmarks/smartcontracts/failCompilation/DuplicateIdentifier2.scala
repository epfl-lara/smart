import stainless.smartcontracts._

object DuplicateIdentifier2 {
    case class DuplicateIdentifier2() extends Contract {
        def foo() = {
            val i: Uint256 = Uint256.ZERO
            if(true) {
                val i: Uint256 = Uint256.ZERO
                true
            } else false
        }
    }
}