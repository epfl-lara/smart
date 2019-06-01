import stainless.smartcontracts._
import stainless.annotation._
import stainless.lang._

trait C1 extends Contract {
  @solidityPure
  @solidityPublic
  final def f() = {
    require(Msg.sender.equals(Address(10)))
  }
}

trait C2 extends Contract {
  @addressOfContract("C1")
  val a: Address

  @solidityView
  @solidityPublic
  final def g() = {
    require(addr.equals(Address(10)))

    Environment.contractAt(a).asInstanceOf[C1].f()
  }

  @solidityPublic
  final def constructor(_balance: Uint256) = {
    // We temporarily use assume here but we must use something
    // that will be compiled so that this fails at runtime if invalid
    ghost(assume(
      Environment.contractAt(a).isInstanceOf[C1] &&
      Environment.contractAt(a).asInstanceOf[C1].addr == a
    ))
  }
}
