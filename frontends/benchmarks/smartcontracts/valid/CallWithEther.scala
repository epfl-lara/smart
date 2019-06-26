import stainless.smartcontracts._
import stainless.smartcontracts.Environment._
import stainless.annotation._
import stainless.lang._

import Environment._

trait CallWithEther extends Contract {
  val other: Address

  @solidityPublic
  final def foo() = {
    dynRequire(
      addr.balance == Uint256("50") &&
      other.balance == Uint256("0")
    )

    pay(unsafeCast[CallWithEther](other).bar(), Uint256("50"))
  }

  @solidityPayable
  @solidityPublic
  final def bar() = {

  }
}
