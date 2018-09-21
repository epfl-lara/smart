import stainless.smartcontracts._

object BasicInterface1 {
    case class BasicInterface1(
    ) extends ContractInterface {
        def foo: Unit

        def bar = true
    }
}