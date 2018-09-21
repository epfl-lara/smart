import stainless.smartcontracts._

object MsgForwarding1 {
    case class MsgForwarding() extends Contract {
        def test() = {
            assert(test2() == Msg.sender)
        }

        def test2() = {
            Msg.sender
        }
    }

}