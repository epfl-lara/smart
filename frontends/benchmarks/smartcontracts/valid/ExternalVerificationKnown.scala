import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._
import stainless.lang._

trait EVKA extends Contract {
  var balance:Uint256

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
    if(balance <= Uint256("250"))
      balance = balance + Uint256.ONE
  }
}

trait EVKB extends Contract {
  var balance:Uint256
  @addressOfContract("EVKA")
  var target:Address

  @solidityPublic
  final def constructor(_balance: Uint256) = {
    // We temporarily use assume here but we must use something
    // that will be compiled so that this fails at runtime if invalid
    ghost(assume(
      Environment.contractAt(target).isInstanceOf[EVKA] &&
      Environment.contractAt(target).asInstanceOf[EVKA].addr == target
    ))

    dynRequire(_balance <= Uint256("120"))
    balance = _balance
  }

  @ghost
  final def invariant() = {
    balance <= Uint256("120")
  }

  @solidityPublic
  final def decrease() = {
    if(balance > Uint256.ZERO)
      balance = balance - Uint256.ONE
  }

  @solidityPublic
  final def exchange() = {
    Environment.contractAt(target).asInstanceOf[EVKA].increase()
    decrease()

    true
  }
}
