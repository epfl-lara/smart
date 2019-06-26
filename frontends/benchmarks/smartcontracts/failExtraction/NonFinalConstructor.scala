import stainless.smartcontracts._

trait NonFinalConstructor extends Contract {
  def constructor(): Unit = ()
}
