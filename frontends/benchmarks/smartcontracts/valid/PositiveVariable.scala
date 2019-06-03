import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._
import stainless.lang.StaticChecks._
import stainless.lang.ghost

trait PositiveVariableUser extends Contract {
  @addressOfContract("PositiveVariable")
  val target: Address

  @solidityPublic
  final def constructor() = {
    // We temporarily use assume here but we must use something
    // that will be compiled so that this fails at runtime if invalid
    ghost(assume(
      Environment.contractAt(target).isInstanceOf[PositiveVariable] &&
      Environment.contractAt(target).asInstanceOf[PositiveVariable].addr == target
    ))
  }

  @solidityPublic
  final def f(): Unit = {
    Environment.contractAt(target).asInstanceOf[PositiveVariable].g()
    assert(Environment.contractAt(target).asInstanceOf[PositiveVariable].getX() >= 0)
  }
}

trait PositiveVariable extends ContractInterface {
  var x: BigInt

  def g(): Unit

  @solidityPublic
  final def constructor(_x:BigInt) = {
    dynRequire(_x >= 0)
    x = _x
  }

  @solidityPublic
  @pure
  final def getX():BigInt = x

  @ghost
  final def invariant(): Boolean = x >= 0
}
