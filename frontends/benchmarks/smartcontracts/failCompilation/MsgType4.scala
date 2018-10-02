import stainless.smartcontracts._

trait MsgType4 extends Contract {
  def foo() = {
    val m: Msg = Msg(address(this), Uint256.ZERO)
  }
}
