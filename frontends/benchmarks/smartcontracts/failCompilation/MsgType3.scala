import stainless.smartcontracts._
import stainless.annotation._

trait MsgType3 extends Contract {
  @extern
  def foo(msg: Msg) = ???
}
