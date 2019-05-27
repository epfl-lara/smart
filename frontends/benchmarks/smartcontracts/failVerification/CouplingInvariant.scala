import stainless.smartcontracts._
import stainless.annotation._

trait CouplingInvariantA extends Contract {
  var balance: Uint256

  @addressOfContract("CouplingInvariantB")
  val target: Address

  @ghost
  final def invariant() = balance >= Environment.contractAt(target).asInstanceOf[CouplingInvariantB].balance
  
  @solidityPublic
  final def constructor() = {
    dynRequire(Environment.contractAt(target).isInstanceOf[CouplingInvariantB] &&
                Environment.contractAt(target).asInstanceOf[CouplingInvariantB].addr == target)

    balance = Environment.contractAt(target).asInstanceOf[CouplingInvariantB].balance
  }
}

trait CouplingInvariantB extends Contract {
  var balance: Uint256

  @solidityPublic
  final def increment() = balance = balance + Uint256.ONE
}
