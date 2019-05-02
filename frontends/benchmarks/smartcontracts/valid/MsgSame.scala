import stainless.smartcontracts._
import stainless.annotation._

trait MsgSame extends Contract {
  @solidityPublic
  def f() = g(Msg.sender)

  @solidityPublic
  def g(a: Address) = {
    require(a.equals(Msg.sender))

  }
}
