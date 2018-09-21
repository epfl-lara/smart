import stainless.smartcontracts._

object MsgType4 {
    case class MsgType4(
    ) extends Contract {
        def foo() = {
            val m: Msg = Msg(address(this), Uint256.ZERO)
        }
    }
}