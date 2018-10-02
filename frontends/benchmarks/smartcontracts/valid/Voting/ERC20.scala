import stainless.smartcontracts._
import stainless.annotation._

trait ERC20 extends ContractInterface {
  @view
  def totalSupply(): Uint256

  @view
  def balanceOf(tokenOwner: Address): Uint256

  @view
  def allowance(tokenOwner: Address, spender: Address): Uint256

  def transfer(to: Address, tokens: Uint256): Boolean
  def approve(spender: Address, tokens: Uint256): Boolean
  def transferFrom(from: Address, to: Address, tokens: Uint256): Boolean
}
