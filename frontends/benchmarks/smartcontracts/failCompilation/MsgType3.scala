import stainless.smartcontracts._
import stainless.annotation._

object MsgType3 {
    case class MsgType3(
    ) extends Contract {
        @extern
        def foo(msg: Msg) = ???
    }
}