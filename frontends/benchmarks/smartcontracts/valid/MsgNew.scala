import stainless.smartcontracts._
import stainless.annotation._

trait C1 extends Contract {
  @solidityPure
  @solidityPublic
  final def f() = {
    require(Msg.sender.equals(Address(10)))
  }
}

trait C2 extends Contract {
  val a: Address

  @ghost
  final def invariant() = Environment.contractAt(a).isInstanceOf[C1]

  @solidityView
  @solidityPublic
  final def g() = {
    require(addr.equals(Address(10)))

    Environment.contractAt(a).asInstanceOf[C1].f()
  }
}
