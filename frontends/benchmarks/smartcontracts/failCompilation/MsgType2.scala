import stainless.smartcontracts._
import stainless.annotation._

trait MsgType2 extends Contract {

  @extern
  def foo(): Msg = ???

}
