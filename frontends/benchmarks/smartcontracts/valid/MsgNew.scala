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
  @addressOfContract("C1")
  val a: Address

  @solidityView
  @solidityPublic
  final def g() = {
    require(addr.equals(Address(10)))

    Environment.contractAt(a).asInstanceOf[C1].f()
  }
}
