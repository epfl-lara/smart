import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._
import stainless.lang._

trait HavocInvariant2B extends Contract {
  var isEmpty:Boolean
  var balance:Uint256

  final def invariant() = (!isEmpty && (balance > Uint256.ZERO)) ||
              (isEmpty && (balance == Uint256.ZERO))

  def emptyContract() = {
    balance = Uint256.ZERO
    isEmpty = true
  }
}

trait HavocInvariant2A extends Contract {
  val target:Address

  final def invariant():Boolean = true

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
    ghost { havoc() }
    assert(Environment.contractAt(target).asInstanceOf[HavocInvariant2B].invariant())
  }

}