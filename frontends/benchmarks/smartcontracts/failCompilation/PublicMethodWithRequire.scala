import stainless.smartcontracts._
import stainless.annotation._

trait PublicMethodRequire extends Contract {
  @solidityPublic
  final def foo() = {
    require(true)

    true
  }
}