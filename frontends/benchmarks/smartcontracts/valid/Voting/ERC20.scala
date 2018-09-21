import stainless.smartcontracts._
import stainless.annotation._

object ERC20 {
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
}

/*
case class Transfer(
  @indexed
  from: Address,
  @indexed
  to: Address,
  tokens: Uint256) extends Event

case class Approval(
  @indexed
  tokenOwner: Address,
  @indexed
  spender: Address,
  tokens: Uint256) extends Event
*/
