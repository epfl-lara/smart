import stainless.smartcontracts._

trait EnvironmentType4 extends Contract {
  def foo() = {
    val e: Environment = Environment(Mapping.constant(Uint256.ZERO))
  }
}
