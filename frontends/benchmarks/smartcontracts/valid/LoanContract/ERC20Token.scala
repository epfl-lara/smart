import stainless.smartcontracts._

import stainless.collection._
import stainless.lang._
import stainless.annotation._

import ERC20Specs._

trait ERC20Token extends ContractInterface {
  // dummy state
  var s: BigInt
  
  // FIXME: use symbol `???` instead of choose
  @library
  def transfer(to: Address, amount: Uint256): Boolean = {
    require(amount >= Uint256.ZERO)
    val oldAddr = this.addr
    val oldBalanceOf = this.balanceOf(_)

    s = s + 1

    val b = choose((b: Boolean) => transferSpec(b, to, Msg.sender, amount, addr, balanceOf, oldAddr, oldBalanceOf))
    b
  } ensuring(res => transferSpec(res, to, Msg.sender, amount, addr, balanceOf, old(this).addr, old(this).balanceOf))

  @library
  def balanceOf(from: Address): Uint256 = {
    choose((b: Uint256) => b >= Uint256.ZERO)
  } ensuring {
    res => old(this).addr == this.addr
  }
}
