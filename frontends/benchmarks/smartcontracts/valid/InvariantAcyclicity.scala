import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._
import stainless.lang.StaticChecks._
import stainless.lang.ghost

trait InvariantAcyclicity1 extends Contract {
  var balance: Uint256

  val target: Address

  @solidityPublic
  final def f(): Unit = {
    assert(balance >= Uint256.ONE)
  }

  @ghost
  final def invariant() =
    balance >= Uint256.ONE

  @solidityPublic
  final def constructor(_balance: Uint256) = {
    dynRequire(_balance >= Uint256.ONE)
    balance = _balance
  }
}

trait InvariantAcyclicity2 extends Contract {
  val target: Address
}
