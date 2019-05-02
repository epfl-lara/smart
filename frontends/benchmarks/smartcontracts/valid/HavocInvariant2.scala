import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._
import stainless.lang.StaticChecks._
import stainless.lang.ghost

trait HavocInvariant2B extends Contract {
  var isEmpty: Boolean
  var balance: Uint256

  @ghost
  @library
  final def invariant() = (!isEmpty && (balance > Uint256.ZERO)) ||
              (isEmpty && (balance == Uint256.ZERO))

  @solidityPublic
  final def emptyContract() = {
    balance = Uint256.ZERO
    isEmpty = true
  }
}

trait HavocInvariant2A extends Contract {
  val target:Address

  @ghost
  @library
  final def invariant(): Boolean = {
    Environment.contractAt(target).isInstanceOf[HavocInvariant2B] &&
    Environment.contractAt(target).asInstanceOf[HavocInvariant2B].invariant()
  }

  @solidityPublic
  final def withdrawBalance() = { 
    Environment.contractAt(target).asInstanceOf[HavocInvariant2B].emptyContract()
    assert(Environment.contractAt(target).asInstanceOf[HavocInvariant2B].invariant())
  }
}