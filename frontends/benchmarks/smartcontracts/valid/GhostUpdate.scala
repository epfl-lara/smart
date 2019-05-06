import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._

trait GhostUpdate extends Contract {
  @ghost var x: BigInt

  def f() = {
    ghost {
      x = x + 1
    }
  }
}