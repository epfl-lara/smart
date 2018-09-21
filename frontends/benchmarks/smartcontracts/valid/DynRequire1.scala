import stainless.smartcontracts._
import stainless.annotation._

object DynRequire1 {
    case class DynRequire1() extends Contract {
        @view
        def foo() = {
            // Solidity require
            dynRequire(address(this).balance == Uint256("42"))

            // Stainless require
            assert(address(this).balance == Uint256("42"))
        }
    }
}