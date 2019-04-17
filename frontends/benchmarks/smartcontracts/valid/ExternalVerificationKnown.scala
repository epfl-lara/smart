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
  final def increase() = {
    if(balance <= Uint256("250"))
      balance = balance + Uint256.ONE
  }
}

trait EVKB extends Contract {
  var balance:Uint256
  var target:Address

  final def invariant() = {
    balance <= Uint256("120")
  }

  @solidityPublic
  final def decrease() = {
    assert(invariant())
    if(balance > Uint256.ZERO)
      balance = balance - Uint256.ONE
  }

  @solidityPublic
  final def exchange() = {
    require(
      Environment.contractAt(target).isInstanceOf[EVKA]
    )

    Environment.contractAt(target).asInstanceOf[EVKA].increase()
    decrease()

    true
  }
}
