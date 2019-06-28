import stainless.smartcontracts._

trait ERC20Token extends ContractInterface {
  def transfer(to: Address, amount: Uint256): Boolean
  def balanceOf(from: Address): Uint256
}
