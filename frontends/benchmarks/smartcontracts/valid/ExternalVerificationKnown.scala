import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._
import stainless.lang.StaticChecks._

import Environment._

trait EVKA extends Contract {
  var balance: Uint256

  @ghost
  final def invariant() = {
    balance >= Uint256("120")
  }

  @solidityPublic
  final def constructor(_balance: Uint256) = {
    dynRequire(_balance >= Uint256("120"))
    balance = _balance
  }

  @solidityPublic
  final def increase() = {
    if (balance <= Uint256("250"))
      balance = balance + Uint256.ONE
  }
}

trait EVKB extends Contract {
  var balance: Uint256
  var target: Address

  @solidityPublic
  final def constructor(_balance: Uint256) = {
    dynRequire(_balance <= Uint256("120"))
    balance = _balance
  }

  @ghost
  final def invariant() = {
    balance <= Uint256("120")
  }

  @solidityPublic
  final def decrease() = {
    if (balance > Uint256.ZERO)
      balance = balance - Uint256.ONE
  }

  @solidityPublic
  final def exchange() = {
    unsafeCast[EVKA](target).increase()
    assert(invariant())
    decrease()

    true
  }
}
