import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._
import stainless.lang.StaticChecks._
import stainless.lang.ghost

trait HavocInvariant2B extends Contract {
  var isEmpty: Boolean
  var balance: Uint256

  @ghost
  final def invariant() = (!isEmpty && (balance > Uint256.ZERO)) ||
              (isEmpty && (balance == Uint256.ZERO))

  @solidityPublic
  final def emptyContract() = {
    balance = Uint256.ZERO
    isEmpty = true
  }

  @solidityPublic
  final def constructor(_balance: Uint256) = {
    balance = _balance
    isEmpty = !(balance > Uint256.ZERO)
  }
}

trait HavocInvariant2A extends Contract {
  @addressOfContract("HavocInvariant2B")
  val target:Address

  @solidityPublic
  final def withdrawBalance() = {
    Environment.contractAt(target).asInstanceOf[HavocInvariant2B].emptyContract()
    assert(Environment.contractAt(target).asInstanceOf[HavocInvariant2B].invariant())
  }

  @solidityPublic
  final def constructor() = {
    // We temporarily use assume here but we must use something
    // that will be compiled so that this fails at runtime if invalid
    ghost(assume(
      Environment.contractAt(target).isInstanceOf[HavocInvariant2B] &&
      Environment.contractAt(target).asInstanceOf[HavocInvariant2B].addr == target
    ))
  }
}