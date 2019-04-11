import stainless.smartcontracts._
import stainless.lang._

trait Havoc extends Contract {
  var x: BigInt

  override final def invariant() = x == 0

  def f() = {
    ghost {
      havoc()
    }
    assert(x == 0)
  }
}
