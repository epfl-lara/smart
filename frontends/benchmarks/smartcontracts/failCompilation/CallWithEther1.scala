import stainless.smartcontracts._
import stainless.annotation._

object CallWithEther1 {
    case class CallWithEther1 (
        val other: CallWithEther1
    ) extends Contract {
        def foo() = {
            require(address(this).balance == Uint256("50") &&
                    address(other).balance == Uint256("0"))

            pay(other.bar(), Uint256("50"))
        } ensuring { _ =>
            address(other).balance == Uint256("50") &&
            address(this).balance == Uint256.ZERO
        }
        
        def bar() = {
            
        }
    }
}