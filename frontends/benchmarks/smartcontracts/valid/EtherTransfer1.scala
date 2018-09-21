import stainless.smartcontracts._

object EtherTransfer1 {
    case class EtherTransfer1 (
        val other: Address
    ) extends Contract {
        def foo(): Unit = {
            require(address(this).balance == Uint256("50") &&
                    other.balance == Uint256("0"))

            other.transfer(Uint256("50"))
        } ensuring { _ =>
            other.balance == Uint256("50") &&
            address(this).balance == Uint256.ZERO
        }
    }
}