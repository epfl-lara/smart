import stainless.smartcontracts._

import stainless.collection._
import stainless.lang._
import stainless.annotation._

@mutable
trait ERC20Token extends ContractInterface {
  def transfer(to: Address, amount: Uint256): Boolean
  def balanceOf(from: Address): Uint256
}
