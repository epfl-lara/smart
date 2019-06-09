import stainless.smartcontracts._
import stainless.annotation._

@solidityLibrary("BasicLibrary")
object BasicLibrary {
  @solidityPure
  @solidityPublic
  def foo() = true

  @solidityPure
  @solidityPublic
  def bar() = false
}
