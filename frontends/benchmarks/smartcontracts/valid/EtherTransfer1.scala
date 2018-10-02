import stainless.smartcontracts._

trait EtherTransfer1 extends Contract {

  val other: Address

  def foo(): Unit = {
      require(address(this).balance == Uint256("50") &&
              other.balance == Uint256("0"))

      other.transfer(Uint256("50"))
  } ensuring { _ =>
      other.balance == Uint256("50") &&
      address(this).balance == Uint256.ZERO
  }
  
}