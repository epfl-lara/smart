import stainless.smartcontracts._
import stainless.annotation._

trait ERC20 extends ContractInterface {
  @solidityView
  def totalSupply(): Uint256

  @solidityView
  def balanceOf(tokenOwner: Address): Uint256

  @solidityView
  def allowance(tokenOwner: Address, spender: Address): Uint256

  def transfer(to: Address, tokens: Uint256): Boolean
  def approve(spender: Address, tokens: Uint256): Boolean
  def transferFrom(from: Address, to: Address, tokens: Uint256): Boolean
}
