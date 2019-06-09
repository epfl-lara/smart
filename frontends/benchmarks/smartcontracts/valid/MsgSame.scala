import stainless.smartcontracts._
import stainless.annotation._

trait MsgSame extends Contract {
  @solidityPublic
  def f() = g(Msg.sender)

  @solidityPrivate
  def g(a: Address) = {
    dynRequire(a == Msg.sender)

  }
}
