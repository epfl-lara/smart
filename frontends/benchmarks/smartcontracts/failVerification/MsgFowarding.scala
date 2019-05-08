import stainless.smartcontracts._
import stainless.annotation._

trait MsgForwarding1 extends Contract {
  @addressOfContract("MsgForwarding2")
  val a: Address

  final def f() = {
    assert(Environment.contractAt(a).asInstanceOf[MsgForwarding2] == Msg.sender)
  }
}

trait MsgForwarding2 extends Contract {
  final def g() = {
    Msg.sender
  }
}
