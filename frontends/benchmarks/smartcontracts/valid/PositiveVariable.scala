import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._
import stainless.lang.StaticChecks._
import stainless.lang.ghost

trait PositiveVariableUser extends Contract {
  var target: Address

  @solidityPublic
  def f(): Unit = {
    Environment.contractAt(target).asInstanceOf[PositiveVariable].g()
    assert(Environment.contractAt(target).asInstanceOf[PositiveVariable].x >= 0)
  }

  @ghost
  final def invariant() = {
    Environment.contractAt(target).isInstanceOf[PositiveVariable] &&
    Environment.contractAt(target).asInstanceOf[PositiveVariable].invariant()
  }
}

trait PositiveVariable extends ContractInterface {
  var x: BigInt

  def g(): Unit

  @ghost
  def invariant(): Boolean = x >= 0
}
