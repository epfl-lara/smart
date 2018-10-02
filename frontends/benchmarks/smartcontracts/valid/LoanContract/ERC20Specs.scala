import stainless.smartcontracts._

import stainless.collection._
import stainless.lang._
import stainless.annotation._

object ERC20Specs {
  def transferUpdate(
    a: Address, 
    to: Address, 
    sender: Address, 
    amount: Uint256, 
    addr: Address,
    balanceOf: Address => Uint256,
    oldAddr: Address,
    oldBalanceOf: Address => Uint256
  ) = {
    ((a == to) ==> (balanceOf(a) == oldBalanceOf(a) + amount)) &&
    ((a == sender) ==> (balanceOf(a) == oldBalanceOf(a) - amount)) &&
    (a != to && a != sender) ==> (balanceOf(a) == oldBalanceOf(a))
  }

  def transferSpec(
    b: Boolean, 
    to: Address, 
    sender: Address, 
    amount: Uint256, 
    addr: Address,
    balanceOf: Address => Uint256,
    oldAddr: Address,
    oldBalanceOf: Address => Uint256
  ) = {
    (!b ==> (balanceOf == oldBalanceOf)) &&
    (b ==> forall((a: Address) => transferUpdate(a, to, sender, amount, addr, balanceOf, oldAddr, oldBalanceOf))) &&
      (addr == oldAddr)
  }
}