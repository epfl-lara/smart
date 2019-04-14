import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._

trait Havoc extends Contract {
  var x: BigInt

  @ghost
  final def invariant() = x == 0

  @solidityPublic
  def f() = {
    ghost {
      havoc()
    }
    assert(x == 0)
  }
}
