import stainless.smartcontracts._
import stainless.annotation._

@solidityLibrary("BasicLibrary1")
object BasicLibrary1 {
  @solidityPure
  @solidityPublic
  def foo() = true

  @solidityPure
  @solidityPublic
  def bar() = false
}
