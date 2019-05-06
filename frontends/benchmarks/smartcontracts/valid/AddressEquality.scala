import stainless.smartcontracts._
import stainless.annotation._

trait AddressEquality extends Contract {
  val other: Address

  def invariant() = 
    other == Address(0) && addr == Address(1) &&
    Environment.contractAt(other).isInstanceOf[AddressEquality] &&
    Environment.contractAt(other).asInstanceOf[AddressEquality].addr == other

  @solidityView
  @solidityPublic
  def foo = {
    assert(other == Address(0))
    assert(addr == Address(1))
    assert(Environment.contractAt(addr).asInstanceOf[AddressEquality].addr == Address(1))
    assert(Environment.contractAt(other).asInstanceOf[AddressEquality].addr == Address(0))
  }
}
