import stainless.smartcontracts._
import stainless.lang._

trait EnvironmentType4 extends Contract {
  def foo() = {
    val e: Environment = Environment(MutableMap.withDefaultValue(Uint256.ZERO))
  }
}
