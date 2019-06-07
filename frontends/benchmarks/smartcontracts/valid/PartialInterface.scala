import stainless.smartcontracts._

trait PartialInterface extends ContractInterface {
  var x: BigInt
  def getX() = x
}
