import stainless.smartcontracts._
import stainless.annotation._

trait MsgSame extends Contract {
  @solidityPublic
  final def f() = g(Msg.sender)

  @solidityPrivate
  final def g(a: Address) = {
    dynRequire(a == Msg.sender)

  }
}
