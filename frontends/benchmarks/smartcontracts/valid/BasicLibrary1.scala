import stainless.smartcontracts._
import stainless.annotation._

@solidityLibrary("BasicLibrary1")
trait BasicLibrary1 {
  @solidityPure
  @solidityPublic
  def foo() = true

  @solidityPure
  @solidityPublic
  def bar() = false
}
