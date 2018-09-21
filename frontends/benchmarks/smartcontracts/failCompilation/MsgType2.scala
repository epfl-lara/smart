import stainless.smartcontracts._
import stainless.annotation._

object MsgType2 {
    case class MsgType2(
    ) extends Contract {
        @extern
        def foo():Msg = ???
    }
}