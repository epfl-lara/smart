import stainless.smartcontracts._
import stainless.annotation._

trait Resend extends Contract {
  @solidityPayable
  @solidityPublic
  final def resend() = {
    val caller = Msg.sender
    val receivedAmount = Msg.value

    caller.transfer(receivedAmount)
  }
}
