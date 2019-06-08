import stainless.smartcontracts._
import stainless.annotation._

trait PrivateMethodInvalid extends Contract {
  @solidityPublic
  final def foo() = {
    require(true)

    true
  }
}