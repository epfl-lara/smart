import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._
import stainless.lang.StaticChecks._
import stainless.lang.ghost

trait InvariantAcyclicity1 extends Contract {
  var balance:Uint256
  
  @addressOfContract("InvariantAcyclicity2")
  val target: Address

  def f(): Unit = {
    assert(balance >= Uint256.ONE)
  }

  def invariant() =
    balance >= Uint256.ONE
}

trait InvariantAcyclicity2 extends Contract {
  @addressOfContract("InvariantAcyclicity1")
  val target: Address
}