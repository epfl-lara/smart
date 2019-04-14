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
  def emptyContract() = {
    balance = Uint256.ZERO
    isEmpty = true
  }
}

trait HavocInvariant2A extends Contract {
  val target:Address

  @ghost
  final def invariant(): Boolean = true

  @solidityPublic
  final def withdrawBalance() = {
    require(
      Environment.contractAt(target).isInstanceOf[HavocInvariant2B]
    )
    // What should we say about the state of the contract B ?
    // Technically if we can prove that after the constructor has been run, B satisfy
    // the invariant and if each method satisfy the invariant then the invariant must be
    // true here.
    Environment.contractAt(target).asInstanceOf[HavocInvariant2B].emptyContract()
    assert(Environment.contractAt(target).asInstanceOf[HavocInvariant2B].invariant())
    assert(Environment.contractAt(target).asInstanceOf[HavocInvariant2B].invariant())
  }

}