import stainless.smartcontracts._
import stainless.annotation._

object EnvironmentType1 {
    case class EnvironmentType1(
        var e1: Environment,
        val e2: Environment
    ) extends Contract {
    }
}