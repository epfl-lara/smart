import stainless.smartcontracts._
import stainless.annotation._

import Environment._

trait DynRequire extends Contract {
  @solidityView
  @solidityPublic
  final def foo() = {
    // Solidity dynamic require
    dynRequire(addr.balance == Uint256("42"))

    // Stainless static assertion
    assert(addr.balance == Uint256("42"))
  }
}
