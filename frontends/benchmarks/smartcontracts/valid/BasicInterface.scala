import stainless.smartcontracts._

trait BasicInterface extends ContractInterface {
  def foo: Unit
  def bar = true
}
