import stainless.smartcontracts._

object EnvironmentType4 {
    case class EnvironmentType4(
    ) extends Contract {
        def foo() = {
            val e: Environment = Environment(Mapping.constant(Uint256.ZERO))
        }
    }
}