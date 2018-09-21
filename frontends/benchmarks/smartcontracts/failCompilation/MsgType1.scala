import stainless.smartcontracts._

object MsgType1 {
    case class MsgType1(
        var m1: Msg,
        val m2: Msg
    ) extends Contract {
    }
}