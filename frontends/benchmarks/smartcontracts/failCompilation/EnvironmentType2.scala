import stainless.smartcontracts._
import stainless.annotation._

object EnvironmentType2 {
    case class EnvironmentType2(
    ) extends Contract {
        @extern
        def foo: Environment = ???
    }
}