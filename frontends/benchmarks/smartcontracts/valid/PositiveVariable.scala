import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._
import stainless.lang.StaticChecks._
import stainless.lang.ghost

trait PositiveVariableUser extends Contract {
  @addressOfContract("PositiveVariable")
  var target: Address

  @solidityPublic
  final def f(): Unit = {
    Environment.contractAt(target).asInstanceOf[PositiveVariable].g()
    assert(Environment.contractAt(target).asInstanceOf[PositiveVariable].x >= 0)
  }
}

trait PositiveVariable extends ContractInterface {
  var x: BigInt

  def g(): Unit

  @ghost
  final def invariant(): Boolean = x >= 0
}
