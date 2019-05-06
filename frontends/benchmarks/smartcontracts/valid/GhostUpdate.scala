import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._

trait GhostUpdate extends Contract {
  @ghost var x: BigInt

  @solidityPublic
  @solidityView
  def f() = {
    ghost {
      x = x + 1
    }
  }
}