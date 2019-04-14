import stainless.smartcontracts._
import stainless.annotation._

trait Resend extends Contract {
  @solidityPayable
  @solidityPublic
  def resend() = {
    val caller = Msg.sender
    val receivedAmount = Msg.value

    caller.transfer(receivedAmount)
  }
}
