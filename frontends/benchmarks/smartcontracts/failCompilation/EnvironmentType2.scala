import stainless.smartcontracts._
import stainless.annotation._

trait EnvironmentType2 extends Contract {
  @extern
  def foo: Environment = ???
}
