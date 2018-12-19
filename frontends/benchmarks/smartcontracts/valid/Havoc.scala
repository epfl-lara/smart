import stainless.smartcontracts._

trait Havoc extends Contract {
  var x: BigInt

  override final def invariant() = x == 0

  def f() = {
    havoc()
    assert(x == 0)
  }
}
