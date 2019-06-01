import stainless.smartcontracts._
import stainless.annotation._
import stainless.lang._

trait AddressEquality extends Contract {
  @addressOfContract("AddressEquality")
  var other: Address

  @ghost
  def invariant() =
    other == Address(0) && addr == Address(1)

  @solidityPublic
  final def constructor() = {
    // We temporarily use assume here but we must use something
    // that will be compiled so that this fails at runtime if invalid
    // Warning : This example should not be used as is.
    ghost(assume(
      Environment.contractAt(other).isInstanceOf[AddressEquality] &&
      Environment.contractAt(other).asInstanceOf[AddressEquality].addr == other &&
      other == Address(0) &&
      addr == Address(1)
    ))
  }

  @solidityView
  @solidityPublic
  def foo = {
    assert(other == Address(0))
    assert(addr == Address(1))
    assert(Environment.contractAt(addr).asInstanceOf[AddressEquality].addr == Address(1))
    assert(Environment.contractAt(other).asInstanceOf[AddressEquality].addr == Address(0))
  }
}
