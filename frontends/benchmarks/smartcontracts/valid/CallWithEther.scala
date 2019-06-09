import stainless.smartcontracts._
import stainless.smartcontracts.Environment._
import stainless.annotation._
import stainless.lang._

import Environment._

trait CallWithEther extends Contract {
  @addressOfContract("CallWithEther")
  val other: Address

  @solidityPublic
  final def constructor() = {
    // We temporarily use assume here but we must use something
    // that will be compiled so that this fails at runtime if invalid
    ghost(dynRequire(
      contractAt(other).isInstanceOf[CallWithEther]
    ))
  }

  @solidityPublic
  def foo() = {
    dynRequire(
      addr.balance == Uint256("50") &&
      other.balance == Uint256("0")
    )

    pay(contractAt(other).asInstanceOf[CallWithEther].bar, Uint256("50"))

    assert(
      other.balance == Uint256("50") &&
      addr.balance == Uint256("0")
    )
  }

  @solidityPayable
  @solidityPublic
  final def bar() = {

  }
}