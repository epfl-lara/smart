import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._
import stainless.lang.StaticChecks._
import stainless.lang.ghost

trait InvariantAcyclicity1 extends Contract {
  var balance:Uint256

  @addressOfContract("InvariantAcyclicity2")
  val target: Address

  @solidityPublic
  def f(): Unit = {
    assert(balance >= Uint256.ONE)
  }

  @ghost
  def invariant() =
    balance >= Uint256.ONE

  @solidityPublic
  final def constructor(_balance: Uint256) = {
    // We temporarily use assume here but we must use something
    // that will be compiled so that this fails at runtime if invalid
    ghost(assume(
      Environment.contractAt(target).isInstanceOf[InvariantAcyclicity2] &&
      Environment.contractAt(target).asInstanceOf[InvariantAcyclicity2].addr == target
    ))

    dynRequire(_balance >= Uint256.ONE)
    balance = _balance
  }
}

trait InvariantAcyclicity2 extends Contract {
  @addressOfContract("InvariantAcyclicity1")
  val target: Address

  @solidityPublic
  final def constructor() = {
    // We temporarily use assume here but we must use something
    // that will be compiled so that this fails at runtime if invalid
    ghost(assume(
      Environment.contractAt(target).isInstanceOf[InvariantAcyclicity1] &&
      Environment.contractAt(target).asInstanceOf[InvariantAcyclicity1].addr == target
    ))
  }
}