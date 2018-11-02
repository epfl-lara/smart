import stainless.smartcontracts._

trait MsgSame extends Contract {
  def f() = g(Msg.sender)

  def g(a: Address) = {
    require(a == Msg.sender)

  }
}
