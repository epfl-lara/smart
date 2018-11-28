import stainless.smartcontracts._

import stainless.collection._
import stainless.lang._
import stainless.annotation._

@mutable
trait ERC20Token extends ContractInterface {
  def transfer(to: Address, amount: Uint256): Boolean
  @pure
  // We assume here that balanceOf is a pure function that doesn't modify any state
  def balanceOf(from: Address): Uint256
}
