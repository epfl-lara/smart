import stainless.smartcontracts._

trait NonFinalMethod extends Contract {
  def f(): Unit = ()
}
