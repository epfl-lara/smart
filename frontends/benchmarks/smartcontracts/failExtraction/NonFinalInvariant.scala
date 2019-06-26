import stainless.smartcontracts._

trait NonFinalInvariant extends Contract {
  def invariant(): Boolean = true
}
