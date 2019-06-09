import stainless.smartcontracts._
import stainless.annotation._
import stainless.lang.ghost

trait CouplingInvariantA extends Contract {
  var balance: Uint256

  @addressOfContract("CouplingInvariantB")
  val target: Address

  @ghost
  final def invariant() = balance >= Environment.contractAt(target).asInstanceOf[CouplingInvariantB].balance

  @solidityPublic
  final def constructor() = {
    ghost(dynRequire(
      Environment.contractAt(target).isInstanceOf[CouplingInvariantB]
    ))

    balance = Environment.contractAt(target).asInstanceOf[CouplingInvariantB].balance
  }
}

trait CouplingInvariantB extends Contract {
  var balance: Uint256

  @solidityPublic
  final def increment() = {
    if(balance <= Uint256("30"))
      balance = balance + Uint256.ONE
  }
}
